/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.struct.specialoperator.go.GoStructFactory;

final class TagbodyFormCollector implements Collector<LispStruct, Map<GoStruct<?>, PrognStruct>, Map<GoStruct<?>, PrognStruct>> {

	private final FormAnalyzer formAnalyzer;

	private final Environment environment;

	private final Map<Class<? extends LispStruct>, GoStructFactory<LispStruct>> goStructGeneratorStrategies;

	private GoStruct<?> currentTag;

	TagbodyFormCollector(final FormAnalyzer formAnalyzer, final Environment environment,
	                     final Map<Class<? extends LispStruct>, GoStructFactory<LispStruct>> goStructGeneratorStrategies) {
		this.formAnalyzer = formAnalyzer;
		this.environment = environment;
		this.goStructGeneratorStrategies = goStructGeneratorStrategies;
		currentTag = null;
	}

	@Override
	public Supplier<Map<GoStruct<?>, PrognStruct>> supplier() {
		// NOTE: use LinkedHashMap so the tags and forms are ordered appropriately
		return LinkedHashMap::new;
	}

	@Override
	public BiConsumer<Map<GoStruct<?>, PrognStruct>, LispStruct> accumulator() {
		return (tagToFormsMap, current) -> {

			final GoStructFactory<LispStruct> goStructFactory = goStructGeneratorStrategies.get(current.getClass());
			if (goStructFactory == null) {
				handleOtherwise(tagToFormsMap, current);
			} else {
				currentTag = goStructFactory.getGoElement(current);
				tagToFormsMap.put(currentTag, new PrognStruct(new ArrayList<>()));
			}
		};
	}

	private void handleOtherwise(final Map<GoStruct<?>, PrognStruct> tagToFormsMap, final LispStruct lispStruct) {
		final LispStruct analyzedForm = formAnalyzer.analyze(lispStruct, environment);
		tagToFormsMap.get(currentTag).getForms().add(analyzedForm);
	}

	@Override
	public BinaryOperator<Map<GoStruct<?>, PrognStruct>> combiner() {
		return (tagToFormsMap1, tagToFormsMap2) -> {
			for (final Map.Entry<GoStruct<?>, PrognStruct> e : tagToFormsMap2.entrySet()) {
				tagToFormsMap1.merge(
						e.getKey(),
						e.getValue(),
						(u, v) -> {
							throw new IllegalStateException(String.format("Duplicate key %s", u));
						}
				);
			}
			return tagToFormsMap1;
		};
	}

	@Override
	public Function<Map<GoStruct<?>, PrognStruct>, Map<GoStruct<?>, PrognStruct>> finisher() {
		return Function.identity();
	}

	@Override
	public Set<Characteristics> characteristics() {
		return Collections.unmodifiableSet(EnumSet.of(Characteristics.IDENTITY_FINISH));
	}
}
