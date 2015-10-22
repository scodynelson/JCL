/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import jcl.LispStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStructGenerator;

final class TagbodyTagSetCollector implements Collector<LispStruct, List<GoStruct<?>>, List<GoStruct<?>>> {

	private final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies;

	TagbodyTagSetCollector(final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies) {
		this.goStructGeneratorStrategies = goStructGeneratorStrategies;
	}

	@Override
	public Supplier<List<GoStruct<?>>> supplier() {
		// NOTE: use ArrayList so the tags are ordered appropriately
		return ArrayList::new;
	}

	@Override
	public BiConsumer<List<GoStruct<?>>, LispStruct> accumulator() {
		return (tagSet, current) -> {

			final GoStructGenerator<LispStruct> goStructGenerator = goStructGeneratorStrategies.get(current.getClass());
			if (goStructGenerator != null) {
				final GoStruct<?> goTag = goStructGenerator.generateGoElement(current);
				tagSet.add(goTag);
			}
		};
	}

	@Override
	public BinaryOperator<List<GoStruct<?>>> combiner() {
		return (left, right) -> {
			left.addAll(right);
			return left;
		};
	}

	@Override
	public Function<List<GoStruct<?>>, List<GoStruct<?>>> finisher() {
		return Function.identity();
	}

	@Override
	public Set<Characteristics> characteristics() {
		return Collections.unmodifiableSet(EnumSet.of(Characteristics.IDENTITY_FINISH));
	}
}
