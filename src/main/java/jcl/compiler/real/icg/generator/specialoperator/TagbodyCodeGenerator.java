package jcl.compiler.real.icg.generator.specialoperator;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.simple.NullCodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyCodeGenerator implements CodeGenerator<TagbodyStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	@Override
	public void generate(final TagbodyStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/GoException");

		mv.visitLabel(tryBlockStart);

		final Map<GoStruct<?>, PrognStruct> tagbodyForms = input.getTagbodyForms();
		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = getTagbodyLabeledForms(tagbodyForms, classBuilder);

		final Set<TagbodyLabel> tagbodyLabels = tagbodyLabeledForms.keySet();
		classBuilder.getTagbodyLabelStack().push(tagbodyLabels);

		for (final Map.Entry<TagbodyLabel, PrognStruct> tagbodyLabeledForm : tagbodyLabeledForms.entrySet()) {
			final TagbodyLabel tagbodyLabel = tagbodyLabeledForm.getKey();
			final Label tagLabel = tagbodyLabel.getLabel();

			mv.visitLabel(tagLabel);

			final PrognStruct forms = tagbodyLabeledForm.getValue();
			prognCodeGenerator.generate(forms, classBuilder);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
		final int goExceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, goExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/GoException", "getTagIndex", "()I", false);
		final int goExceptionIndexStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, goExceptionIndexStore);

		mv.visitVarInsn(Opcodes.ILOAD, goExceptionIndexStore);

		// NOTE: the 'tagbodyLabelList' here will be properly ordered because the 'tagbodyLabels' are an ordered LinkedKeySet
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
		final Label defaultLabel = new Label();
		mv.visitTableSwitchInsn(minKey, maxKey, defaultLabel, tagLabels);

		mv.visitLabel(defaultLabel);
		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);
	}

	private Map<TagbodyLabel, PrognStruct> getTagbodyLabeledForms(final Map<GoStruct<?>, PrognStruct> tagbodyForms,
	                                                              final JavaClassBuilder classBuilder) {

		// NOTE: use LinkedHashMap so the tags and forms are ordered appropriately
		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = new LinkedHashMap<>();
		for (final Map.Entry<GoStruct<?>, PrognStruct> tagbodyForm : tagbodyForms.entrySet()) {
			final GoStruct<?> tag = tagbodyForm.getKey();
			final PrognStruct forms = tagbodyForm.getValue();

			final int nextTagbodyTagIndex = classBuilder.getNextTagbodyTagIndex();
			final TagbodyLabel tagbodyLabel = new TagbodyLabel(tag, nextTagbodyTagIndex, new Label());
			tagbodyLabeledForms.put(tagbodyLabel, forms);
		}

		return tagbodyLabeledForms;
	}
}
