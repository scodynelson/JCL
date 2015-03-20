package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockCodeGenerator implements CodeGenerator<BlockStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final BlockStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "block", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException");

		final SymbolStruct<?> name = input.getName();

		final Label namePackage = new Label();
		mv.visitLabel(namePackage);
//		mv.visitLineNumber(19, namePackage);
		final String packageName = name.getSymbolPackage().getName();
		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label nameSymbol = new Label();
		mv.visitLabel(nameSymbol);
//		mv.visitLineNumber(20, nameSymbol);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		final String symbolName = name.getName();
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(tryBlockStart);
//		mv.visitLineNumber(24, tryBlockStart);
		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		mv.visitLabel(tryBlockEnd);
//		mv.visitLineNumber(31, tryBlockEnd);
		final Label catchBlockEnd = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
//		mv.visitLineNumber(25, catchBlock);
		mv.visitFrame(Opcodes.F_FULL, 2, new Object[]{"jcl/packages/PackageStruct", "jcl/symbols/SymbolStruct"}, 1, new Object[]{"jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException"});
		mv.visitVarInsn(Opcodes.ASTORE, 4);

		final Label getReturnFromName = new Label();
		mv.visitLabel(getReturnFromName);
//		mv.visitLineNumber(26, getReturnFromName);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "getName", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 5);

		final Label checkEqualsReturnFromNameAndBlockName = new Label();
		mv.visitLabel(checkEqualsReturnFromNameAndBlockName);
//		mv.visitLineNumber(27, checkEqualsReturnFromNameAndBlockName);
		mv.visitVarInsn(Opcodes.ALOAD, 5);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "equals", "(Ljava/lang/Object;)Z", false);

		final Label setResultValue = new Label();
		mv.visitJumpInsn(Opcodes.IFNE, setResultValue);

		final Label rethrowReturnFromException = new Label();
		mv.visitLabel(rethrowReturnFromException);
//		mv.visitLineNumber(28, rethrowReturnFromException);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(setResultValue);
//		mv.visitLineNumber(30, setResultValue);
		mv.visitFrame(Opcodes.F_APPEND, 3, new Object[]{Opcodes.TOP, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "jcl/symbols/SymbolStruct"}, 0, null);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "getResult", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		mv.visitLabel(catchBlockEnd);
//		mv.visitLineNumber(32, catchBlockEnd);
		mv.visitFrame(Opcodes.F_FULL, 3, new Object[]{"jcl/packages/PackageStruct", "jcl/symbols/SymbolStruct", "jcl/LispStruct"}, 0, new Object[]{});
		mv.visitVarInsn(Opcodes.ALOAD, 3);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("pkg", "Ljcl/packages/PackageStruct;", null, nameSymbol, localVariables, 1);
		mv.visitLocalVariable("name", "Ljcl/symbols/SymbolStruct;", "Ljcl/symbols/SymbolStruct<*>;", tryBlockStart, localVariables, 2);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, tryBlockEnd, catchBlock, 3);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, catchBlockEnd, localVariables, 3);
		mv.visitLocalVariable("rte", "Ljcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException;", null, getReturnFromName, catchBlockEnd, 4);
		mv.visitLocalVariable("rteName", "Ljcl/symbols/SymbolStruct;", "Ljcl/symbols/SymbolStruct<*>;", checkEqualsReturnFromNameAndBlockName, catchBlockEnd, 5);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 5);
		mv.visitEnd();
	}
}
