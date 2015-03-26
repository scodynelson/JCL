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
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();
		final Environment currentEnvironment = bindingStack.peek();

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
		}

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
	}
}
