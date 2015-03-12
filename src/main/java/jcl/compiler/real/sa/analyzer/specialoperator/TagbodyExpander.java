package jcl.compiler.real.sa.analyzer.specialoperator;

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
import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStructGenerator;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyExpander extends MacroFunctionExpander {

	private static final long serialVersionUID = -1543233114989622747L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Resource
	private Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.TAGBODY.setMacroFunctionExpander(this);
	}

	@Override
	public TagbodyStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		ListStruct body = form.getRest();
		List<LispStruct> bodyAsJavaList = body.getAsJavaList();

		final Set<GoStruct<?>> currentTagSet = bodyAsJavaList.stream()
		                                                     .collect(new TagbodyInitialTagCollector());

		analysisBuilder.getTagbodyStack().push(currentTagSet);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		if (!isTagbodyTag(body.getFirst())) {
			final SymbolStruct<?> defaultFormsTag = new SymbolStruct<>("Tag" + UUID.randomUUID());
			body = new ConsStruct(defaultFormsTag, body);
			bodyAsJavaList = body.getAsJavaList();
		}

		try {
			final Map<LispStruct, List<LispStruct>> tagbodyForms = bodyAsJavaList.stream()
			                                                                     .collect(new TagbodyCollector(formAnalyzer, analysisBuilder));
			return new TagbodyStruct(tagbodyForms);
		} finally {
			analysisBuilder.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	private final class TagbodyInitialTagCollector implements Collector<LispStruct, Set<GoStruct<?>>, Set<GoStruct<?>>> {

		@Override
		public Supplier<Set<GoStruct<?>>> supplier() {
			return HashSet::new;
		}

		@Override
		public BiConsumer<Set<GoStruct<?>>, LispStruct> accumulator() {
			return (goElementSet, lispStruct) -> {

				final GoStructGenerator<LispStruct> goElementGenerator = goStructGeneratorStrategies.get(lispStruct.getClass());
				if (goElementGenerator != null) {
					final GoStruct<?> goElement = goElementGenerator.generateGoElement(lispStruct);
					goElementSet.add(goElement);
				}
			};
		}

		@Override
		public BinaryOperator<Set<GoStruct<?>>> combiner() {
			return (left, right) -> {
				left.addAll(right);
				return left;
			};
		}

		@Override
		public Function<Set<GoStruct<?>>, Set<GoStruct<?>>> finisher() {
			return Function.identity();
		}

		@Override
		public Set<Characteristics> characteristics() {
			return Collections.unmodifiableSet(EnumSet.of(Collector.Characteristics.UNORDERED, Collector.Characteristics.IDENTITY_FINISH));
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}

	private final class TagbodyCollector implements Collector<LispStruct, Map<LispStruct, List<LispStruct>>, Map<LispStruct, List<LispStruct>>> {

		private final FormAnalyzer analyzer;

		private final AnalysisBuilder analysisBuilder;

		private GoStruct<?> currentTag;

		private TagbodyCollector(final FormAnalyzer analyzer, final AnalysisBuilder analysisBuilder) {
			this.analyzer = analyzer;
			this.analysisBuilder = analysisBuilder;
			currentTag = null;
		}

		private void handleOtherwise(final Map<LispStruct, List<LispStruct>> lispStructListMap, final LispStruct lispStruct) {
			if (!lispStructListMap.containsKey(currentTag)) {
				lispStructListMap.put(currentTag, new ArrayList<>());
			}

			final LispStruct analyzedForm = analyzer.analyze(lispStruct, analysisBuilder);
			lispStructListMap.get(currentTag).add(analyzedForm);
		}

		@Override
		public Supplier<Map<LispStruct, List<LispStruct>>> supplier() {
			return HashMap::new;
		}

		@Override
		public BiConsumer<Map<LispStruct, List<LispStruct>>, LispStruct> accumulator() {
			return (elementListMap, lispStruct) -> {

				final GoStructGenerator<LispStruct> goElementGenerator = goStructGeneratorStrategies.get(lispStruct.getClass());
				if (goElementGenerator == null) {
					handleOtherwise(elementListMap, lispStruct);
				} else {
					currentTag = goElementGenerator.generateGoElement(lispStruct);
				}
			};
		}

		@Override
		public BinaryOperator<Map<LispStruct, List<LispStruct>>> combiner() {
			return (elementListMap, elementListMap2) -> {
				for (Map.Entry<LispStruct, List<LispStruct>> e : elementListMap2.entrySet()) {
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
		public Function<Map<LispStruct, List<LispStruct>>, Map<LispStruct, List<LispStruct>>> finisher() {
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
