package jcl.compiler.real.icg.generator;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.printer.Printer;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class GoCodeGenerator extends SpecialOperatorCodeGenerator<GoStruct<?>> {

	@Autowired
	private Printer printer;

	private static final String GO_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(GoException.class, int.class);

	private GoCodeGenerator() {
		super("go");
	}

	@Override
	protected void generateSpecialOperator(final GoStruct<?> input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Stack<Set<TagbodyLabel>> tagbodyLabelStack = generatorState.getTagbodyLabelStack();
		final TagbodyLabel tagbodyLabel = getTagbodyLabel(tagbodyLabelStack, input);

		final int index = tagbodyLabel.getIndex();
		mv.visitLdcInsn(index);
		final int tagIndexStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, tagIndexStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.GO_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ILOAD, tagIndexStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.GO_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GO_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
	}

	private TagbodyLabel getTagbodyLabel(final Stack<Set<TagbodyLabel>> tagbodyLabelStack, final GoStruct<?> tagToFind) {

		final ListIterator<Set<TagbodyLabel>> tagbodyLabelListIterator = tagbodyLabelStack.listIterator(tagbodyLabelStack.size());

		TagbodyLabel tagbodyLabel = null;

		out:
		while (tagbodyLabelListIterator.hasPrevious()) {
			final Set<TagbodyLabel> previousStack = tagbodyLabelListIterator.previous();
			for (final TagbodyLabel currentTBL : previousStack) {
				final GoStruct<?> goTag = currentTBL.getTag();
				if (tagToFind.equals(goTag)) {
					tagbodyLabel = currentTBL;
					break out;
				}
			}
		}

		if (tagbodyLabel == null) {
			final LispStruct tagObject = tagToFind.getTag();
			final String printedTagObject = printer.print(tagObject);
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + printedTagObject + " is visible.");
		}

		return tagbodyLabel;
	}
}
