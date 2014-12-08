package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class TagbodyAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		final HashMap<LispStruct, SymbolStruct<?>> currentTagMap = readAllTagbodyTags(bodyJavaList);

		analysisBuilder.getTagbodyStack().push(currentTagMap);

		try {
			final List<LispStruct> newBodyJavaList =
					bodyJavaList.stream()
					            .map(e -> getBodyElement(e, currentTagMap, analyzer, analysisBuilder))
					            .collect(Collectors.toList());

			final List<LispStruct> tagbodyResultList = new ArrayList<>();
			tagbodyResultList.add(SpecialOperator.TAGBODY);
			tagbodyResultList.addAll(newBodyJavaList);

			return ListStruct.buildProperList(tagbodyResultList);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static LispStruct getBodyElement(final LispStruct bodyElement, final HashMap<LispStruct, SymbolStruct<?>> currentTagMap,
	                                         final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder) {
		if (isTagbodyTag(bodyElement)) {
			return currentTagMap.get(bodyElement);
		} else {
			return analyzer.analyzeForm(bodyElement, analysisBuilder);
		}
	}

	private static HashMap<LispStruct, SymbolStruct<?>> readAllTagbodyTags(final List<LispStruct> bodyJavaList) {
		final HashMap<LispStruct, SymbolStruct<?>> currentTagMap = new HashMap<>();

		// This is bad practice. HOWEVER!!! We CANNOT do a non-destructive version due to Type Inference of the SymbolStruct. Bleh!!!!
		bodyJavaList
				.stream()
				.filter(TagbodyAnalyzer::isTagbodyTag)
				.forEach(current -> {
					final SymbolStruct<?> newSym = new SymbolStruct<>("Tagbody_Tag_" + UUID.randomUUID());
					currentTagMap.put(current, newSym);
				});

		return currentTagMap;
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof NumberStruct);
	}
}
