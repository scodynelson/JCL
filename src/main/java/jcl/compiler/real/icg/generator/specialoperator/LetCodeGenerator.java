package jcl.compiler.real.icg.generator.specialoperator;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
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

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label nonExceptionFinally = new Label();
		final Label catchBlock = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, nonExceptionFinally, catchBlock, null);

		final Label exceptionFinally = new Label();
		mv.visitTryCatchBlock(catchBlock, exceptionFinally, catchBlock, null);

		final List<LetStruct.LetVar> vars = input.getVars();
		final List<Integer> varSymbolStores = new ArrayList<>(vars.size());

		for (final LetStruct.LetVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();

			final String packageName = symbolVar.getSymbolPackage().getName();
			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			final int packageStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			final String symbolName = symbolVar.getName();
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			final int varSymbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, varSymbolStore);

			// Add here so we can unbind the initForms later
			varSymbolStores.add(varSymbolStore);

			final LispStruct initForm = var.getInitForm();
			formGenerator.generate(initForm, classBuilder);
			final int initFormStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);
		}

		mv.visitLabel(tryBlockStart);

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, classBuilder);

		final int resultStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		// Start: Finally Non-exception
		mv.visitLabel(nonExceptionFinally);
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
		}
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);
		// Start: Finally Non-exception

		// Start: Catch
		mv.visitLabel(catchBlock);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);
		// End: Catch

		// Start: Finally Exception
		mv.visitLabel(exceptionFinally);
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
		}

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);
		// End: Finally Exception

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);
	}
}
