package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.TagbodyElement;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

@Component
public class TagbodyAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		ListStruct body = input.getRest();

		List<LispStruct> bodyJavaList = body.getAsJavaList();
		final Set<LispStruct> currentTagSet = bodyJavaList.stream()
		                                                  .filter(TagbodyAnalyzer::isTagbodyTag)
		                                                  .collect(Collectors.toSet());

		analysisBuilder.getTagbodyStack().push(currentTagSet);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		if (!isTagbodyTag(body.getFirst())) {
			final SymbolStruct<?> defaultFormsTag = new SymbolStruct<>("Tag" + UUID.randomUUID());
			body = new ConsStruct(defaultFormsTag, body);
			bodyJavaList = body.getAsJavaList();
		}

		try {
			final Map<LispStruct, List<LispStruct>> tagbodyForms = bodyJavaList.stream()
			                                                                   .collect(new TagbodyCollector(analyzer, analysisBuilder));
			return new TagbodyElement(tagbodyForms);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof NumberStruct);
	}

	private static class TagbodyCollector implements Collector<LispStruct, Map<LispStruct, List<LispStruct>>, Map<LispStruct, List<LispStruct>>> {

		private final SemanticAnalyzer analyzer;

		private final AnalysisBuilder analysisBuilder;

		private LispStruct currentTag;

		private TagbodyCollector(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder) {
			this.analyzer = analyzer;
			this.analysisBuilder = analysisBuilder;
			currentTag = null;
		}

		@Override
		public Supplier<Map<LispStruct, List<LispStruct>>> supplier() {
			return HashMap::new;
		}

		@Override
		public BiConsumer<Map<LispStruct, List<LispStruct>>, LispStruct> accumulator() {
			return (lispStructListMap, lispStruct) -> {

				if (isTagbodyTag(lispStruct)) {
					currentTag = lispStruct;
					return;
				}

				if (!lispStructListMap.containsKey(currentTag)) {
					lispStructListMap.put(currentTag, new ArrayList<>());
				}

				final LispStruct analyzedForm = analyzer.analyzeForm(lispStruct, analysisBuilder);
				lispStructListMap.get(currentTag).add(analyzedForm);
			};
		}

		@Override
		public BinaryOperator<Map<LispStruct, List<LispStruct>>> combiner() {
			return (lispStructListMap, lispStructListMap2) -> {
				for (Map.Entry<LispStruct, List<LispStruct>> e : lispStructListMap2.entrySet()) {
					lispStructListMap.merge(
							e.getKey(),
							e.getValue(),
							(u, v) -> {
								throw new IllegalStateException(String.format("Duplicate key %s", u));
							});
				}
				return lispStructListMap;
			};
		}

		@Override
		public Function<Map<LispStruct, List<LispStruct>>, Map<LispStruct, List<LispStruct>>> finisher() {
			return Function.identity();
		}

		@Override
		public Set<Characteristics> characteristics() {
			return Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.IDENTITY_FINISH));
		}
	}
}
