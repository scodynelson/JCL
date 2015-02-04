package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.IntegerElement;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.compiler.real.sa.element.specialoperator.GoElement;
import jcl.compiler.real.sa.element.specialoperator.GoIntegerElement;
import jcl.compiler.real.sa.element.specialoperator.GoSymbolElement;
import jcl.compiler.real.sa.element.specialoperator.TagbodyElement;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;
import jcl.util.InstanceOf;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

@Component
public class TagbodyAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -1543233114989622747L;

	@Override
	public TagbodyElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		ListStruct body = input.getRest();

		List<LispStruct> bodyJavaList = body.getAsJavaList();
		final Set<GoElement> currentTagSet = bodyJavaList.stream()
		                                                 .collect(new TagbodyInitialTagCollector());

		analysisBuilder.getTagbodyStack().push(currentTagSet);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		if (!isTagbodyTag(body.getFirst())) {
			final SymbolStruct<?> defaultFormsTag = new SymbolStruct<>("Tag" + UUID.randomUUID());
			body = new ConsStruct(defaultFormsTag, body);
			bodyJavaList = body.getAsJavaList();
		}

		try {
			final Map<Element, List<Element>> tagbodyForms = bodyJavaList.stream()
			                                                             .collect(new TagbodyCollector(analyzer, analysisBuilder));
			return new TagbodyElement(tagbodyForms);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	private static final class TagbodyInitialTagCollector implements Collector<LispStruct, Set<GoElement>, Set<GoElement>> {

		@Override
		public Supplier<Set<GoElement>> supplier() {
			return HashSet::new;
		}

		private GoElement getGoSymbolElementTag(final SymbolStruct<?> symbolStruct) {
			final SymbolElement<?> symbolElement = new SymbolElement<>(symbolStruct);
			return new GoSymbolElement(symbolElement);
		}

		private GoElement getGoIntegerElementTag(final IntegerStruct integerStruct) {
			final IntegerElement integerElement = new IntegerElement(integerStruct);
			return new GoIntegerElement(integerElement);
		}

		@Override
		public BiConsumer<Set<GoElement>, LispStruct> accumulator() {
			return (goElementSet, lispStruct) -> {
				final Optional<GoElement> goElement
						= InstanceOf.when(lispStruct)
						            .isInstanceOf(SymbolStruct.class).thenReturn(this::getGoSymbolElementTag)
						            .isInstanceOf(IntegerStruct.class).thenReturn(this::getGoIntegerElementTag)
						            .get();
				if (goElement.isPresent()) {
					final GoElement goElementVal = goElement.get();
					goElementSet.add(goElementVal);
				}
			};
		}

		@Override
		public BinaryOperator<Set<GoElement>> combiner() {
			return (left, right) -> {
				left.addAll(right);
				return left;
			};
		}

		@Override
		public Function<Set<GoElement>, Set<GoElement>> finisher() {
			return Function.identity();
		}

		@Override
		public Set<Characteristics> characteristics() {
			return Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
		}
	}

	private static final class TagbodyCollector implements Collector<LispStruct, Map<Element, List<Element>>, Map<Element, List<Element>>> {

		private final SemanticAnalyzer analyzer;

		private final AnalysisBuilder analysisBuilder;

		private GoElement currentTag;

		private TagbodyCollector(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder) {
			this.analyzer = analyzer;
			this.analysisBuilder = analysisBuilder;
			currentTag = null;
		}

		private void updateCurrentTagToSymbol(final SymbolStruct<?> symbolStruct) {
			final SymbolElement<?> symbolElement = new SymbolElement<>(symbolStruct);
			currentTag = new GoSymbolElement(symbolElement);
		}

		private void updateCurrentTagToInteger(final IntegerStruct integerStruct) {
			final IntegerElement integerElement = new IntegerElement(integerStruct);
			currentTag = new GoIntegerElement(integerElement);
		}

		private void handleOtherwise(final Map<Element, List<Element>> lispStructListMap, final LispStruct lispStruct) {
			if (!lispStructListMap.containsKey(currentTag)) {
				lispStructListMap.put(currentTag, new ArrayList<>());
			}

			final Element analyzedForm = analyzer.analyzeForm(lispStruct, analysisBuilder);
			lispStructListMap.get(currentTag).add(analyzedForm);
		}

		@Override
		public Supplier<Map<Element, List<Element>>> supplier() {
			return HashMap::new;
		}

		@Override
		public BiConsumer<Map<Element, List<Element>>, LispStruct> accumulator() {
			return (elementListMap, lispStruct) ->
					InstanceOf.when(lispStruct)
					          .isInstanceOf(SymbolStruct.class).then(this::updateCurrentTagToSymbol)
					          .isInstanceOf(IntegerStruct.class).then(this::updateCurrentTagToInteger)
					          .otherwise(e -> handleOtherwise(elementListMap, e));
		}

		@Override
		public BinaryOperator<Map<Element, List<Element>>> combiner() {
			return (elementListMap, elementListMap2) -> {
				for (Map.Entry<Element, List<Element>> e : elementListMap2.entrySet()) {
					elementListMap.merge(
							e.getKey(),
							e.getValue(),
							(u, v) -> {
								throw new IllegalStateException(String.format("Duplicate key %s", u));
							});
				}
				return elementListMap;
			};
		}

		@Override
		public Function<Map<Element, List<Element>>, Map<Element, List<Element>>> finisher() {
			return Function.identity();
		}

		@Override
		public Set<Characteristics> characteristics() {
			return Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.IDENTITY_FINISH));
		}
	}
}
