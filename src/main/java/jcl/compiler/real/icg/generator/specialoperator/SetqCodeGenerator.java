package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqCodeGenerator implements CodeGenerator<SetqStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final SetqStruct input, final JavaClassBuilder classBuilder) {

		final List<SetqStruct.SetqPair> setqPairs = input.getSetqPairs();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();
		final MethodVisitor previousMv = currentClass.getMethodVisitor();

		final String setqMethodName = "setq_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, setqMethodName, "()Ljcl/LispStruct;", null, null);
		currentClass.setMethodVisitor(mv);
		mv.visitCode();

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();
		final Environment currentEnvironment = bindingStack.peek();

		final Integer closureBindingsStore = currentClass.getNextAvailableStore();

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "getClosure", "()Ljcl/functions/Closure;", false);
		final Integer closureStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitVarInsn(Opcodes.ASTORE, closureBindingsStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		final Label closureNullCheckIfEnd = new Label();
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/Closure", "getClosureBindings", "()Ljava/util/Map;", false);
		mv.visitVarInsn(Opcodes.ASTORE, closureBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final int packageStore = currentClass.getNextAvailableStore();
		final int symbolStore = currentClass.getNextAvailableStore();
		final int initFormStore = currentClass.getNextAvailableStore();

		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			final SymbolStruct<?> var = setqPair.getVar();
			final LispStruct form = setqPair.getForm();

			final String packageName = var.getSymbolPackage().getName();
			final String symbolName = var.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

			formGenerator.generate(form, classBuilder);
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

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(var);
			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);

			if (hasLexicalBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "setLexicalValue", "(Ljcl/LispStruct;)V", false);
			} else if (hasDynamicBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "setDynamicValue", "(Ljcl/LispStruct;)V", false);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "setValue", "(Ljcl/LispStruct;)V", false);
			}

			mv.visitVarInsn(Opcodes.ALOAD, closureBindingsStore);
			final Label closureBindingsNullCheckIfEnd = new Label();
			mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureBindingsNullCheckIfEnd);
		}

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		currentClass.setMethodVisitor(previousMv);

		previousMv.visitVarInsn(Opcodes.ALOAD, 0);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, setqMethodName, "()Ljcl/LispStruct;", false);
	}
}
