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
import java.util.Map;
import java.util.UUID;

@Component
public class TagbodyAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		final Map<LispStruct, SymbolStruct<?>> currentTagMap = readAllTagbodyTags(bodyJavaList);

		analysisBuilder.getTagbodyStack().push(currentTagMap);

		try {
			final List<LispStruct> newBodyJavaList = new ArrayList<>();

			for (final LispStruct currentBodyElement : bodyJavaList) {
				if (isTagbodyTag(currentBodyElement)) {
					final SymbolStruct<?> realTagSymbol = currentTagMap.get(currentBodyElement);
					newBodyJavaList.add(realTagSymbol);
				} else {
					final LispStruct analyzedElement = analyzer.analyzeForm(currentBodyElement, analysisBuilder);
					newBodyJavaList.add(analyzedElement);
				}
			}

			final List<LispStruct> tagbodyResultList = new ArrayList<>();
			tagbodyResultList.add(SpecialOperator.TAGBODY);
			tagbodyResultList.addAll(newBodyJavaList);

			return ListStruct.buildProperList(tagbodyResultList);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static Map<LispStruct, SymbolStruct<?>> readAllTagbodyTags(final List<LispStruct> bodyJavaList) {
		final HashMap<LispStruct, SymbolStruct<?>> currentTagMap = new HashMap<>();

		bodyJavaList
				.stream()
				.filter(TagbodyAnalyzer::isTagbodyTag)
				.forEach(current -> {
					final SymbolStruct<?> newSym = new SymbolStruct<>("Tagbody_Tag_" + UUID.randomUUID());
					currentTagMap.put(current, newSym);
				});

		return currentTagMap;
	}

	private static boolean isTagbodyTag(final LispStruct current) {
		return (current instanceof SymbolStruct) || (current instanceof NumberStruct);
	}
}
