package jcl.compiler.real.icg.generator.specialoperator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetCodeGenerator implements CodeGenerator<LetStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LetStruct input, final JavaClassBuilder classBuilder) {

		final List<LetStruct.LetVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final LetEnvironment letEnvironment = input.getLetEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final int packageStore = currentClass.getNextAvailableStore();

		final Map<Integer, Integer> lexicalSymbolStoresToBind = new HashMap<>();
		final Map<Integer, Integer> dynamicSymbolStoresToBind = new HashMap<>();

		final Set<Integer> lexicalSymbolStoresToUnbind = lexicalSymbolStoresToBind.keySet();
		final Set<Integer> dynamicSymbolStoresToUnbind = dynamicSymbolStoresToBind.keySet();

		for (final LetStruct.LetVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();
			final LispStruct initForm = var.getInitForm();
			final boolean isSpecial = var.isSpecial();

			final String packageName = symbolVar.getSymbolPackage().getName();
			final String symbolName = symbolVar.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the initForms later
			final int symbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

			formGenerator.generate(initForm, classBuilder);
			final int initFormStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			final Label valuesCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
			mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
			final int valuesStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitLabel(valuesCheckIfEnd);

			if (isSpecial) {
				dynamicSymbolStoresToBind.put(symbolStore, initFormStore);
			} else {
				lexicalSymbolStoresToBind.put(symbolStore, initFormStore);
			}
		}

		final Integer parentClosureStore;

		final Stack<Integer> closureStoreStack = currentClass.getClosureStoreStack();
		if (closureStoreStack.isEmpty()) {
			mv.visitVarInsn(Opcodes.ALOAD, 0); // TODO: I know that '0' essentially means 'this'. But can we do better by passing the actual Store value around???
			final String fileName = currentClass.getFileName();
			mv.visitFieldInsn(Opcodes.GETFIELD, fileName, "closure", "Ljcl/functions/Closure;");
			parentClosureStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, parentClosureStore);
		} else {
			parentClosureStore = closureStoreStack.peek();
		}

		mv.visitTypeInsn(Opcodes.NEW, "jcl/functions/Closure");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, parentClosureStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/functions/Closure", "<init>", "(Ljcl/functions/Closure;)V", false);
		final Integer newClosureStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, newClosureStore);

		closureStoreStack.push(newClosureStore);

		for (final Map.Entry<Integer, Integer> functionStoreToBind : lexicalSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, newClosureStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/Closure", "getClosureBindings", "()Ljava/util/Map;", false);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", true);
			mv.visitInsn(Opcodes.POP);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : dynamicSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindDynamicValue", "(Ljcl/LispStruct;)V", false);
		}

		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(letEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		// Pop closure store after we get through the body forms
		closureStoreStack.pop();

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
	}

	private void generateFinallyCode(final MethodVisitor mv, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                 final Set<Integer> dynamicSymbolStoresToUnbind) {
		for (final Integer symbolStore : lexicalSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
		}

		for (final Integer var : dynamicSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, var);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindDynamicValue", "()V", false);
		}
	}
}
