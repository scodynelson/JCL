package jcl.compiler.real.icg.generator;

import java.security.SecureRandom;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.SymbolMacroletStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class SymbolMacroletCodeGenerator implements CodeGenerator<SymbolMacroletStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final SymbolMacroletStruct input, final GeneratorState generatorState) {

		final List<SymbolMacroletStruct.SymbolMacroletVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final Environment symbolMacroletEnvironment = input.getSymbolMacroletEnvironment();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final Set<Integer> symbolVarStores = new HashSet<>(vars.size());

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int expansionStore = methodBuilder.getNextAvailableStore();

		for (final SymbolMacroletStruct.SymbolMacroletVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the expansions later
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(symbolVar, methodBuilder, packageStore, symbolStore);

			// Add the symbolStore here so we can unbind the expansions later
			symbolVarStores.add(symbolStore);

			final LispStruct expansion = var.getExpansion();
			final String symbolMacroExpanderClassName = generateSymbolMacroExpander(expansion, generatorState);

			mv.visitTypeInsn(Opcodes.NEW, symbolMacroExpanderClassName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, symbolMacroExpanderClassName, "<init>", "()V", false);
			mv.visitVarInsn(Opcodes.ASTORE, expansionStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, expansionStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindSymbolMacroExpander", "(Ljcl/compiler/real/sa/analyzer/expander/SymbolMacroExpander;)V", false);
		}

		mv.visitLabel(tryBlockStart);

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(symbolMacroletEnvironment);
		prognCodeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, symbolVarStores);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, symbolVarStores);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
	}

	private static void generateFinallyCode(final MethodVisitor mv, final Set<Integer> varSymbolStores) {
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindSymbolMacroExpander", "()V", false);
		}
	}

	private String generateSymbolMacroExpander(final LispStruct expansion, final GeneratorState classBuilder) {

		final String fileName = "SymbolMacrolet" + '_' + System.nanoTime();
		final String className = "jcl/" + fileName;

		final JavaClassBuilder currentClass = new JavaClassBuilder(className, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = classBuilder.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		classBuilder.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();
		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, className, null, "jcl/compiler/real/sa/analyzer/expander/SymbolMacroExpander", null);

		cw.visitSource(fileName + ".java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Deque<JavaMethodBuilder> methodBuilderDeque = classBuilder.getMethodBuilderDeque();
			methodBuilderDeque.addFirst(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/sa/analyzer/expander/SymbolMacroExpander", "<init>", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderDeque.removeFirst();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "expand", "(Ljcl/LispStruct;Ljcl/compiler/real/environment/Environment;)Ljcl/LispStruct;", null, null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Deque<JavaMethodBuilder> methodBuilderDeque = classBuilder.getMethodBuilderDeque();
			methodBuilderDeque.addFirst(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int expressionStore = methodBuilder.getNextAvailableStore();
			final int environmentStore = methodBuilder.getNextAvailableStore();

			codeGenerator.generate(expansion, classBuilder);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderDeque.removeFirst();
		}
		cw.visitEnd();

		classBuilderDeque.removeFirst();

		return className;
	}
}
