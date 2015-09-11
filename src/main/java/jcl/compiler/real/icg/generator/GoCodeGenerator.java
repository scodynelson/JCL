package jcl.compiler.real.icg.generator;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'go' special operator code generation.
 */
@Component
final class GoCodeGenerator extends SpecialOperatorCodeGenerator<GoStruct<?>> {

	/**
	 * Private constructor which passes 'go' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private GoCodeGenerator() {
		super("go");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link GoStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the appropriate tag index by searching the {@link GeneratorState#tagbodyLabelStack} for the
	 * {@link TagbodyLabel#getTag()} matching the provided {@link GoStruct}</li>
	 * <li>Creating and throwing a new {@link GoException} with the {@code int} tag index value</li>
	 * </ol>
	 * As an example, it will transform {@code (go 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct go_1(Closure var1) {
	 *      int var2 = 1;
	 *      throw new GoException(var2);
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link GoStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	protected void generateSpecialOperator(final GoStruct<?> input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		// Generate the Tag Index
		final TagbodyLabel tagbodyLabel = getTagbodyLabel(generatorState, input);
		final int tagIndex = tagbodyLabel.getIndex();
		mv.visitLdcInsn(tagIndex);

		final int tagIndexStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, tagIndexStore);

		// Create and throw the GoException
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.GO_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ILOAD, tagIndexStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.GO_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.GO_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
	}

	/**
	 * Private method to retrieve the {@link TagbodyLabel} corresponding to the provided {@link GoStruct} tag within
	 * the current execution stack from the provided {@link GeneratorState}. This is accomplished by iterating through
	 * each {@link Set<TagbodyLabel>} within the {@link GeneratorState#tagbodyLabelStack} until the tag equivalent to
	 * the provided {@link GoStruct} tag is located.
	 *
	 * @param generatorState
	 * 		the {@link GeneratorState} used to retrieve the {@link TagbodyLabel} corresponding to the provided {@link
	 * 		GoStruct} tag
	 * @param tagToFind
	 * 		the {@link GoStruct} tag used to located the corresponding {@link TagbodyLabel} within the {@link
	 * 		GeneratorState#tagbodyLabelStack}
	 *
	 * @return the {@link TagbodyLabel} corresponding to the provided {@link GoStruct} tag
	 */
	private static TagbodyLabel getTagbodyLabel(final GeneratorState generatorState, final GoStruct<?> tagToFind) {

		final Stack<Set<TagbodyLabel>> tagbodyLabelStack = generatorState.getTagbodyLabelStack();
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

		// NOTE: This value should NEVER be 'null' in reality. This check is done inside the GoExpander.
		return tagbodyLabel;
	}
}
