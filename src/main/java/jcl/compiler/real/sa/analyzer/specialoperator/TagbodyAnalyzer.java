package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.TagbodyElement;
import jcl.compiler.real.element.specialoperator.go.GoElement;
import jcl.compiler.real.element.specialoperator.go.GoElementGenerator;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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

	@Resource
	private Map<Class<? extends SimpleElement>, GoElementGenerator<SimpleElement>> goElementGeneratorStrategies;

	@Override
	public TagbodyElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final EnhancedLinkedList<SimpleElement> body = elements.getAllButFirst();

		final Set<GoElement<?>> currentTagSet = body.stream()
		                                            .collect(new TagbodyInitialTagCollector());

		analysisBuilder.getTagbodyStack().push(currentTagSet);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		if (!isTagbodyTag(body.getFirst())) {
			final SymbolElement defaultFormsTag = new SymbolElement(null, "Tag" + UUID.randomUUID());
			body.addFirst(defaultFormsTag);
		}

		try {
			final Map<Element, List<Element>> tagbodyForms = body.stream()
			                                                     .collect(new TagbodyCollector(analyzer, analysisBuilder));
			return new TagbodyElement(tagbodyForms);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final SimpleElement element) {
		return (element instanceof SymbolElement) || (element instanceof IntegerElement);
	}

	private final class TagbodyInitialTagCollector implements Collector<SimpleElement, Set<GoElement<?>>, Set<GoElement<?>>> {

		@Override
		public Supplier<Set<GoElement<?>>> supplier() {
			return HashSet::new;
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}		@Override
		public BiConsumer<Set<GoElement<?>>, SimpleElement> accumulator() {
			return (goElementSet, lispStruct) -> {

				final GoElementGenerator<SimpleElement> goElementGenerator = goElementGeneratorStrategies.get(lispStruct.getClass());
				if (goElementGenerator != null) {
					final GoElement<?> goElement = goElementGenerator.generateGoElement(lispStruct);
					goElementSet.add(goElement);
				}
			};
		}

		@Override
		public BinaryOperator<Set<GoElement<?>>> combiner() {
			return (left, right) -> {
				left.addAll(right);
				return left;
			};
		}

		@Override
		public Function<Set<GoElement<?>>, Set<GoElement<?>>> finisher() {
			return Function.identity();
		}

		@Override
		public Set<Characteristics> characteristics() {
			return Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
		}


	}

	private final class TagbodyCollector implements Collector<SimpleElement, Map<Element, List<Element>>, Map<Element, List<Element>>> {

		private final SemanticAnalyzer analyzer;

		private final AnalysisBuilder analysisBuilder;

		private GoElement<?> currentTag;

		private TagbodyCollector(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder) {
			this.analyzer = analyzer;
			this.analysisBuilder = analysisBuilder;
			currentTag = null;
		}

		private void handleOtherwise(final Map<Element, List<Element>> lispStructListMap, final SimpleElement lispStruct) {
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
		public BiConsumer<Map<Element, List<Element>>, SimpleElement> accumulator() {
			return (elementListMap, lispStruct) -> {

				final GoElementGenerator<SimpleElement> goElementGenerator = goElementGeneratorStrategies.get(lispStruct.getClass());
				if (goElementGenerator == null) {
					handleOtherwise(elementListMap, lispStruct);
				} else {
					currentTag = goElementGenerator.generateGoElement(lispStruct);
				}
			};
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

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
