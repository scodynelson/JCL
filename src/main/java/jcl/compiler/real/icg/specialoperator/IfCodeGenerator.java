package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.system.EnhancedLinkedList;
import org.objectweb.asm.Label;

public class IfCodeGenerator implements CodeGenerator<ConsElement> {

	public static final IfCodeGenerator INSTANCE = new IfCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {

		EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();
		final Object testObj = restOfList.getFirst();
		restOfList = restOfList.getAllButFirst();
		final Object thenObj = restOfList.getFirst();
		restOfList = restOfList.getAllButFirst();
		final Object elseObj = restOfList.getFirst();

		codeGenerator.icgMainLoop(testObj);
		final Label outLabel = new Label();
		codeGenerator.emitter.emitDup();
		codeGenerator.emitter.emitInstanceof("[Ljava/lang/Object;");
		codeGenerator.emitter.emitIfeq(outLabel);
		codeGenerator.emitter.emitCheckcast("[Ljava/lang/Object;");
		codeGenerator.emitter.emitLdc(0);
		codeGenerator.emitter.emitAaload();
		codeGenerator.emitter.visitMethodLabel(outLabel);
		codeGenerator.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		final Label thenLabel = new Label();
		final Label elseLabel = new Label();
		final Label endLabel = new Label();

		codeGenerator.emitter.emitIf_acmpeq(elseLabel);
		codeGenerator.emitter.visitMethodLabel(thenLabel);
		codeGenerator.icgMainLoop(thenObj);
		codeGenerator.emitter.emitGoto(endLabel);

		codeGenerator.emitter.visitMethodLabel(elseLabel);
		codeGenerator.icgMainLoop(elseObj);

		codeGenerator.emitter.visitMethodLabel(endLabel);
	}
}
