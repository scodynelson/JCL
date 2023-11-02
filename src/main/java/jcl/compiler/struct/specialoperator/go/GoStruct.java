/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import java.util.Deque;
import java.util.Iterator;
import java.util.Set;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.icg.generator.GoException;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class GoStruct<T extends LispStruct> extends CompilerSpecialOperatorStruct {

	private final T tag;

	protected GoStruct(final T tag) {
		super("go");
		this.tag = tag;
	}

	@Override
	public boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof GoStruct)
						&& tag.eq(((GoStruct<?>) object).tag));
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(GO ");

		final String printedTag = tag.toString();
		builder.append(printedTag);

		builder.append(')');

		return builder.toString();
	}

	/**
	 * {@inheritDoc} Generation method for {@code GoStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the appropriate tag index by searching the {@link GeneratorState#tagbodyLabelDeque} for the
	 * {@link GeneratorState.TagbodyLabel#tag} matching the provided {@code GoStruct}</li>
	 * <li>Creating and throwing a new {@link GoException} with the {@code int} tag index value</li>
	 * </ol>
	 * As an example, it will transform {@code (go 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct go_1(Environment var1) {
	 *      int var2 = 1;
	 *      throw new GoException(var2);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 *        {@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		// Generate the Tag Index
		final GeneratorState.TagbodyLabel tagbodyLabel = getTagbodyLabel(generatorState, this);
		if (tagbodyLabel == null) {
			throw new IllegalStateException("Missing Tagbody Label");
		}

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
	 * Private method to retrieve the {@link GeneratorState.TagbodyLabel} corresponding to the provided {@code GoStruct}
	 * tag within the current execution stack from the provided {@link GeneratorState}. This is accomplished by
	 * iterating through each {@link Set} within the {@link GeneratorState#tagbodyLabelDeque} until the tag equivalent
	 * to the provided {@code GoStruct} tag is located.
	 *
	 * @param generatorState
	 * 		the {@link GeneratorState} used to retrieve the {@link GeneratorState.TagbodyLabel} corresponding to the
	 * 		provided {@code GoStruct} tag
	 * @param tagToFind
	 * 		the {@code GoStruct} tag used to located the corresponding {@link GeneratorState.TagbodyLabel} within the
	 *        {@link GeneratorState#tagbodyLabelDeque}
	 *
	 * @return the {@link GeneratorState.TagbodyLabel} corresponding to the provided {@code GoStruct} tag
	 */
	private static GeneratorState.TagbodyLabel getTagbodyLabel(final GeneratorState generatorState, final GoStruct<?> tagToFind) {

		final Deque<Set<GeneratorState.TagbodyLabel>> tagbodyLabelDeque = generatorState.getTagbodyLabelDeque();
		final Iterator<Set<GeneratorState.TagbodyLabel>> tagbodyLabelIterator = tagbodyLabelDeque.iterator();

		GeneratorState.TagbodyLabel tagbodyLabel = null;

		out:
		while (tagbodyLabelIterator.hasNext()) {
			final Set<GeneratorState.TagbodyLabel> previousStack = tagbodyLabelIterator.next();
			for (final GeneratorState.TagbodyLabel currentTBL : previousStack) {
				final GoStruct<?> goTag = currentTBL.getTag();
				if (tagToFind.eql(goTag)) {
					tagbodyLabel = currentTBL;
					break out;
				}
			}
		}

		// NOTE: This value should NEVER be 'null' in reality. This check is done inside the GoExpander.
		return tagbodyLabel;
	}
}
