/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.TagbodyStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.NILStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'tagbody' special operator code generation.
 */
@Component
final class TagbodyCodeGenerator extends SpecialOperatorCodeGenerator<TagbodyStruct> {

	/**
	 * {@link PrognCodeGenerator} used for generating the values of the {@link TagbodyStruct#tagbodyForms}.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'tagbody' as the prefix value to be set in it's {@link #methodNamePrefix}
	 * value.
	 */
	private TagbodyCodeGenerator() {
		super("tagbody");
	}

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<TagbodyStruct> event) {
		super.onGeneratorEvent(event);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link TagbodyStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Initializing a try-catch block</li>
	 * <li>Generating each of the {@link TagbodyStruct#tagbodyForms} inside the try block, each separated by a custom
	 * defined {@link Label} indicating the branch point</li>
	 * <li>Catching the expected {@link GoException}</li>
	 * <li>Grabbing the {@link GoException#tagIndex} {@code int} and using it to 'switch' on to know where to make a
	 * branch label jump into the appropriate 'tagbody' form within either the previously generated 'try{}' block or a
	 * 'try{}' block of an outer scoped 'tagbody'</li>
	 * <li>If the {@code int} tag index can be switched to, the case statement will jump to the appropriate label
	 * within the previously generated 'try{}' block</li>
	 * <li>If the {@code int} tag index cannot be switched to, the {@link GoException} is re-thrown</li>
	 * </ol>
	 * As an rough example, it will transform {@code (tagbody)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct tagbody_1(Closure var1) {
	 *      while(true) {
	 *          try {
	 *              NILStruct var10000 = NILStruct.INSTANCE;
	 *          } catch (GoException var4) {
	 *              int var3 = var4.getTagIndex();
	 *              switch(var3) {
	 *                  case 0:
	 *                      continue;
	 *                  default:
	 *                      throw var4;
	 *              }
	 *          }
	 *          return NILStruct.INSTANCE;
	 *      }
	 * }
	 * }
	 * </pre>
	 * NOTE: For any 'tagbody' calls that utilize the 'go' branching procedure, there is no equatable Java code to
	 * represent what bytecode is generated due to branching to labels that are only present at the bytecode level.
	 *
	 * @param input
	 * 		the {@link TagbodyStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final TagbodyStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.GO_EXCEPTION_NAME);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		final Map<GoStruct<?>, PrognStruct> tagbodyForms = input.getTagbodyForms();
		final Map<GeneratorState.TagbodyLabel, PrognStruct> tagbodyLabeledForms = getTagbodyLabeledForms(tagbodyForms, generatorState);

		final Set<GeneratorState.TagbodyLabel> tagbodyLabels = tagbodyLabeledForms.keySet();
		generatorState.getTagbodyLabelDeque().addFirst(tagbodyLabels);

		// Create a label for each set of 'progn' body forms and generate the 'progn' body forms, popping the final result
		// after each generation
		for (final Map.Entry<GeneratorState.TagbodyLabel, PrognStruct> tagbodyLabeledForm : tagbodyLabeledForms.entrySet()) {
			final GeneratorState.TagbodyLabel tagbodyLabel = tagbodyLabeledForm.getKey();
			final PrognStruct forms = tagbodyLabeledForm.getValue();

			final Label tagLabel = tagbodyLabel.getLabel();
			mv.visitLabel(tagLabel);

			codeGenerator.generate(forms, generatorState);
			mv.visitInsn(Opcodes.POP);
		}

		// End 'try{}'
		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		// Start 'catch(GoException ge){}'
		mv.visitLabel(catchBlockStart);
		final int goExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, goExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.GO_EXCEPTION_NAME,
		                   GenerationConstants.GO_EXCEPTION_GET_TAG_INDEX_METHOD_NAME,
		                   GenerationConstants.GO_EXCEPTION_GET_TAG_INDEX_METHOD_DESC,
		                   false);
		final int goExceptionIndexStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, goExceptionIndexStore);

		// NOTE: The 'tagbodyLabelList' here will be properly ordered because the 'tagbodyLabels' are an ordered LinkedKeySet
		//       thanks to the usage of 'LinkedHashMap'
		final List<GeneratorState.TagbodyLabel> tagbodyLabelList = new ArrayList<>(tagbodyLabels);

		final int tagsSize = tagbodyLabelList.size();
		final int[] tagNumbers = new int[tagsSize];
		final Label[] tagLabels = new Label[tagsSize];

		for (int index = 0; index < tagsSize; index++) {
			final GeneratorState.TagbodyLabel indexedLabel = tagbodyLabelList.get(index);

			tagNumbers[index] = indexedLabel.getIndex();
			tagLabels[index] = indexedLabel.getLabel();
		}

		mv.visitVarInsn(Opcodes.ILOAD, goExceptionIndexStore);

		final int minKey = tagNumbers[0];
		final int maxKey = tagNumbers[tagsSize - 1];
		final Label defaultCase = new Label();
		mv.visitTableSwitchInsn(minKey, maxKey, defaultCase, tagLabels);

		mv.visitLabel(defaultCase);
		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		// End 'catch(GoException ge){}'
		mv.visitLabel(catchBlockEnd);
		codeGenerator.generate(NILStruct.INSTANCE, generatorState);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Private method for creating a {@link Map} of {@link GeneratorState.TagbodyLabel}s to corresponding {@link
	 * PrognStruct}s. The provided {@code tagbodyForms} is iterated over, grabbing each {@link GoStruct} tag, fetching
	 * the next available tag index via {@link GeneratorState#getNextTagbodyTagIndex()}, and creating a new {@link
	 * Label} to comprise each newly created {@link GeneratorState.TagbodyLabel}. From here, each {@link
	 * GeneratorState.TagbodyLabel} is then added to a {@link LinkedHashMap} with the corresponding {@link PrognStruct}
	 * as the entry.
	 *
	 * @param tagbodyForms
	 * 		the original {@link Map} of {@link GoStruct} tags to {@link PrognStruct}s to convert to the {@link Map} of
	 * 		{@link GeneratorState.TagbodyLabel}s to the same {@link PrognStruct}s
	 * @param generatorState
	 * 		the {@link GeneratorState} used to retrieve the next available tag index
	 *
	 * @return a {@link Map} of {@link GeneratorState.TagbodyLabel}s to corresponding {@link PrognStruct}s
	 */
	private static Map<GeneratorState.TagbodyLabel, PrognStruct> getTagbodyLabeledForms(final Map<GoStruct<?>, PrognStruct> tagbodyForms,
	                                                                                    final GeneratorState generatorState) {

		// NOTE: use LinkedHashMap so the tags and forms are ordered appropriately
		final Map<GeneratorState.TagbodyLabel, PrognStruct> tagbodyLabeledForms = new LinkedHashMap<>();
		for (final Map.Entry<GoStruct<?>, PrognStruct> tagbodyForm : tagbodyForms.entrySet()) {
			final GoStruct<?> tag = tagbodyForm.getKey();
			final PrognStruct forms = tagbodyForm.getValue();

			final int nextTagbodyTagIndex = generatorState.getNextTagbodyTagIndex();
			final GeneratorState.TagbodyLabel tagbodyLabel = new GeneratorState.TagbodyLabel(tag, nextTagbodyTagIndex, new Label());
			tagbodyLabeledForms.put(tagbodyLabel, forms);
		}

		return tagbodyLabeledForms;
	}
}
