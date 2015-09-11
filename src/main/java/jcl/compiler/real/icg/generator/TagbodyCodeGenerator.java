package jcl.compiler.real.icg.generator;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
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
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * {@link NullCodeGenerator} used for generating a {@link NullStruct} at the end of the created 'tagbody' method as
	 * the result.
	 */
	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	/**
	 * Private constructor which passes 'tagbody' as the prefix value to be set in it's {@link #methodNamePrefix}
	 * value.
	 */
	private TagbodyCodeGenerator() {
		super("tagbody");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link TagbodyStruct} objects. As an example, it will transform {@code (tagbody)} into
	 * the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct tagbody_1(Closure var1) {
	 *      while(true) {
	 *          try {
	 *              NullStruct var10000 = NullStruct.INSTANCE;
	 *          } catch (GoException var4) {
	 *              int var3 = var4.getTagIndex();
	 *              switch(var3) {
	 *                  case 0:
	 *                      continue;
	 *                  default:
	 *                      throw var4;
	 *              }
	 *          }
	 *          return NullStruct.INSTANCE;
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link TagbodyStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
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

		final Map<GoStruct<?>, PrognStruct> tagbodyForms = input.getTagbodyForms();
		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = getTagbodyLabeledForms(tagbodyForms, generatorState);

		final Set<TagbodyLabel> tagbodyLabels = tagbodyLabeledForms.keySet();
		generatorState.getTagbodyLabelStack().push(tagbodyLabels);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		// Create a label for each set of 'progn' body forms and generate the 'progn' body forms, popping the final result
		// after each generation
		for (final Map.Entry<TagbodyLabel, PrognStruct> tagbodyLabeledForm : tagbodyLabeledForms.entrySet()) {
			final TagbodyLabel tagbodyLabel = tagbodyLabeledForm.getKey();
			final PrognStruct forms = tagbodyLabeledForm.getValue();

			final Label tagLabel = tagbodyLabel.getLabel();
			mv.visitLabel(tagLabel);

			prognCodeGenerator.generate(forms, generatorState);
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
		final List<TagbodyLabel> tagbodyLabelList = new ArrayList<>(tagbodyLabels);

		final int tagsSize = tagbodyLabelList.size();
		final int[] tagNumbers = new int[tagsSize];
		final Label[] tagLabels = new Label[tagsSize];

		for (int index = 0; index < tagsSize; index++) {
			final TagbodyLabel indexedLabel = tagbodyLabelList.get(index);

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
		nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Private method for creating a {@link Map} of {@link TagbodyLabel}s to corresponding {@link PrognStruct}s. The
	 * provided {@code tagbodyForms} is iterated over, grabbing each {@link GoStruct} tag, fetching the next available
	 * tag index via {@link GeneratorState#getNextTagbodyTagIndex()}, and creating a new {@link Label} to comprise each
	 * newly created {@link TagbodyLabel}. From here, each {@link TagbodyLabel} is then added to a {@link
	 * LinkedHashMap} with the corresponding {@link PrognStruct} as the entry.
	 *
	 * @param tagbodyForms
	 * 		the original {@link Map} of {@link GoStruct} tags to {@link PrognStruct}s to convert to the {@link Map} of
	 * 		{@link TagbodyLabel}s to the same {@link PrognStruct}s
	 * @param generatorState
	 * 		the {@link GeneratorState} used to retrieve the next available tag index
	 *
	 * @return a {@link Map} of {@link TagbodyLabel}s to corresponding {@link PrognStruct}s
	 */
	private static Map<TagbodyLabel, PrognStruct> getTagbodyLabeledForms(final Map<GoStruct<?>, PrognStruct> tagbodyForms,
	                                                                     final GeneratorState generatorState) {

		// NOTE: use LinkedHashMap so the tags and forms are ordered appropriately
		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = new LinkedHashMap<>();
		for (final Map.Entry<GoStruct<?>, PrognStruct> tagbodyForm : tagbodyForms.entrySet()) {
			final GoStruct<?> tag = tagbodyForm.getKey();
			final PrognStruct forms = tagbodyForm.getValue();

			final int nextTagbodyTagIndex = generatorState.getNextTagbodyTagIndex();
			final TagbodyLabel tagbodyLabel = new TagbodyLabel(tag, nextTagbodyTagIndex, new Label());
			tagbodyLabeledForms.put(tagbodyLabel, forms);
		}

		return tagbodyLabeledForms;
	}
}
