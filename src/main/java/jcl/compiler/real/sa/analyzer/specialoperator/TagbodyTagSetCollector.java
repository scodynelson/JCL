/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
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
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class TagbodyTagSetCollector implements Collector<LispStruct, Set<GoStruct<?>>, Set<GoStruct<?>>> {

	private final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies;

	TagbodyTagSetCollector(final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies) {
		this.goStructGeneratorStrategies = goStructGeneratorStrategies;
	}

	@Override
	public Supplier<Set<GoStruct<?>>> supplier() {
		return HashSet::new;
	}

	@Override
	public BiConsumer<Set<GoStruct<?>>, LispStruct> accumulator() {
		return (tagSet, current) -> {

			final GoStructGenerator<LispStruct> goStructGenerator = goStructGeneratorStrategies.get(current.getClass());
			if (goStructGenerator != null) {
				final GoStruct<?> goTag = goStructGenerator.generateGoElement(current);
				tagSet.add(goTag);
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
		return Collections.unmodifiableSet(EnumSet.of(Characteristics.UNORDERED, Characteristics.IDENTITY_FINISH));
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
