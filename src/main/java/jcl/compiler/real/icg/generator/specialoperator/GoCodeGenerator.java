package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoCodeGenerator implements CodeGenerator<GoStruct<?>> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final GoStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "goGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label getTagValue = new Label();
		mv.visitLabel(getTagValue);
//		mv.visitLineNumber(134, getTagValue);
		final LispStruct tag = input.getTag();
		formGenerator.generate(tag, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label throwGoException = new Label();
		mv.visitLabel(throwGoException);
//		mv.visitLineNumber(136, throwGoException);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/old/exception/GoException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/old/exception/GoException", "<init>", "(Ljava/lang/Object;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("tag", "Ljcl/LispStruct;", null, throwGoException, localVariables, 1);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 2);
		mv.visitEnd();
	}

	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		/* Get the symbol out of the list. */
		final ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) restOfList.getFirst();

        /*
		// first, try to see if this is a go in the same tagbody (the most common)
        TagbodyLabel tbl = findTagbodyBySymbol(tagbodyStack.peek(), sym);
        if (tbl != null) {
        emitter.emitGoto(tbl.label);
        } else {
         */
		/* Throw a GoException. */
		classBuilder.getEmitter().emitNew("lisp/system/compiler/exceptions/GoException");
		classBuilder.getEmitter().emitDup();
		//genCodeSpecialSymbol(sym);
		classBuilder.getEmitter().emitLdc(String.valueOf(TagbodyCodeGenerator.findTagbodyInStack(classBuilder.getTagbodyStack(), sym).getIndex()));   // me
		//emitter.emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Llisp/common/type/Symbol;)V"); //me
		classBuilder.getEmitter().emitInvokespecial("lisp/system/compiler/exceptions/GoException", "<init>", "(Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
		//}
	}
}
