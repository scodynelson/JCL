package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import org.objectweb.asm.Label;

public class IfCodeGenerator {

	public static void genCodeIf(final IntermediateCodeGenerator icg, ListStruct list) {

		list = list.getRest();
		final Object testObj = list.getFirst();
		list = list.getRest();
		final Object thenObj = list.getFirst();
		list = list.getRest();
		final Object elseObj = list.getFirst();

		icg.icgMainLoop(testObj);
		final Label outLabel = new Label();
		icg.emitter.emitDup();
		icg.emitter.emitInstanceof("[Ljava/lang/Object;");
		icg.emitter.emitIfeq(outLabel);
		icg.emitter.emitCheckcast("[Ljava/lang/Object;");
		icg.emitter.emitLdc(0);
		icg.emitter.emitAaload();
		icg.emitter.visitMethodLabel(outLabel);
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		final Label thenLabel = new Label();
		final Label elseLabel = new Label();
		final Label endLabel = new Label();

		icg.emitter.emitIf_acmpeq(elseLabel);
		icg.emitter.visitMethodLabel(thenLabel);
		icg.icgMainLoop(thenObj);
		icg.emitter.emitGoto(endLabel);

		icg.emitter.visitMethodLabel(elseLabel);
		icg.icgMainLoop(elseObj);

		icg.emitter.visitMethodLabel(endLabel);
	}
}
