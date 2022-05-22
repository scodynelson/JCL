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

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.struct.specialoperator.go.GoSymbolStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

final class TagbodyFormCollector implements Collector<LispStruct, Map<GoStruct<?>, PrognStruct>, Map<GoStruct<?>, PrognStruct>> {

	private final Environment environment;

	private GoStruct<?> currentTag;

	TagbodyFormCollector(final Environment environment) {
		this.environment = environment;
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

			if (current instanceof SymbolStruct) {
				currentTag = new GoSymbolStruct((SymbolStruct) current);
				tagToFormsMap.put(currentTag, new PrognStruct(new ArrayList<>()));
			} else if (current instanceof IntegerStruct) {
				currentTag = new GoIntegerStruct((IntegerStruct) current);
				tagToFormsMap.put(currentTag, new PrognStruct(new ArrayList<>()));
			} else {
				handleOtherwise(tagToFormsMap, current);
			}
		};
	}

	private void handleOtherwise(final Map<GoStruct<?>, PrognStruct> tagToFormsMap, final LispStruct lispStruct) {
		final LispStruct analyzedForm = FormAnalyzer.analyze(lispStruct, environment);
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
