package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromCodeGenerator implements CodeGenerator<ReturnFromStruct> {

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ReturnFromStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "returnFrom", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final SymbolStruct<?> name = input.getName();

		final Label namePackage = new Label();
		mv.visitLabel(namePackage);
//		mv.visitLineNumber(38, namePackage);
		final String packageName = name.getSymbolPackage().getName();
		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label nameSymbol = new Label();
		mv.visitLabel(nameSymbol);
//		mv.visitLineNumber(39, nameSymbol);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		final String symbolName = name.getName();
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label getResultValue = new Label();
		mv.visitLabel(getResultValue);
//		mv.visitLineNumber(41, getResultValue);
		final LispStruct result = input.getResult();
		formGenerator.generate(result, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		final Label throwReturnFromException = new Label();
		mv.visitLabel(throwReturnFromException);
//		mv.visitLineNumber(43, throwReturnFromException);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "<init>", "(Ljcl/symbols/SymbolStruct;Ljcl/LispStruct;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("pkg", "Ljcl/packages/PackageStruct;", null, nameSymbol, localVariables, 1);
		mv.visitLocalVariable("name", "Ljcl/symbols/SymbolStruct;", "Ljcl/symbols/SymbolStruct<*>;", getResultValue, localVariables, 2);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, throwReturnFromException, localVariables, 3);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(4, 3);
		mv.visitEnd();
	}
}
