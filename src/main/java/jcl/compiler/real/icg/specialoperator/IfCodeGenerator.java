package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import org.objectweb.asm.Label;

public class IfCodeGenerator implements CodeGenerator<ListStruct> {

	public static final IfCodeGenerator INSTANCE = new IfCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		ListStruct restOfList = input.getRest();
		final Object testObj = restOfList.getFirst();
		restOfList = restOfList.getRest();
		final Object thenObj = restOfList.getFirst();
		restOfList = restOfList.getRest();
		final Object elseObj = restOfList.getFirst();

		codeGenerator.icgMainLoop(testObj, classBuilder);
		final Label outLabel = new Label();
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInstanceof("[Ljava/lang/Object;");
		classBuilder.getEmitter().emitIfeq(outLabel);
		classBuilder.getEmitter().emitCheckcast("[Ljava/lang/Object;");
		classBuilder.getEmitter().emitLdc(0);
		classBuilder.getEmitter().emitAaload();
		classBuilder.getEmitter().visitMethodLabel(outLabel);
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		final Label thenLabel = new Label();
		final Label elseLabel = new Label();
		final Label endLabel = new Label();

		classBuilder.getEmitter().emitIf_acmpeq(elseLabel);
		classBuilder.getEmitter().visitMethodLabel(thenLabel);
		codeGenerator.icgMainLoop(thenObj, classBuilder);
		classBuilder.getEmitter().emitGoto(endLabel);

		classBuilder.getEmitter().visitMethodLabel(elseLabel);
		codeGenerator.icgMainLoop(elseObj, classBuilder);

		classBuilder.getEmitter().visitMethodLabel(endLabel);
	}
}
