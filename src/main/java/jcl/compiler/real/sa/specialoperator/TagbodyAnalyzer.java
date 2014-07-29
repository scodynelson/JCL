package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.UUID;

public class TagbodyAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final TagbodyAnalyzer INSTANCE = new TagbodyAnalyzer();

	public static final Stack<Map<LispStruct, SymbolStruct<?>>> TAGBODY_STACK = new Stack<>();

	@Override
	public ListStruct analyze(final ListStruct input) {

		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		final Map<LispStruct, SymbolStruct<?>> currentTagMap = readAllTagbodyTags(bodyJavaList);

		TAGBODY_STACK.push(currentTagMap);

		try {
			final List<LispStruct> newBodyJavaList = new ArrayList<>();

			for (final LispStruct currentBodyElement : bodyJavaList) {
				if (isTagbodyTag(currentBodyElement)) {
					final SymbolStruct<?> realTagSymbol = currentTagMap.get(currentBodyElement);
					newBodyJavaList.add(realTagSymbol);
				} else {
					final LispStruct analyzedElement = SemanticAnalyzer.saMainLoop(currentBodyElement);
					newBodyJavaList.add(analyzedElement);
				}
			}

			final List<LispStruct> tagbodyResultList = new ArrayList<>();
			tagbodyResultList.add(SpecialOperator.TAGBODY);
			tagbodyResultList.addAll(newBodyJavaList);

			return ListStruct.buildProperList(tagbodyResultList);
		} finally {
			TAGBODY_STACK.pop();
		}
	}

	private static Map<LispStruct, SymbolStruct<?>> readAllTagbodyTags(final List<LispStruct> bodyJavaList) {
		final Map<LispStruct, SymbolStruct<?>> currentTagMap = new HashMap<>();

		bodyJavaList.stream().filter(TagbodyAnalyzer::isTagbodyTag).forEach(current -> {
			final SymbolStruct<?> newSym = new SymbolStruct<>("Tagbody_Tag_" + UUID.randomUUID());
			currentTagMap.put(current, newSym);
		});

		return currentTagMap;
	}

	private static boolean isTagbodyTag(final LispStruct current) {
		return (current instanceof SymbolStruct) || (current instanceof NumberStruct);
	}
}
