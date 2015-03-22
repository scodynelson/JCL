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
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(goStructGeneratorStrategies)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final TagbodyTagSetCollector rhs = (TagbodyTagSetCollector) obj;
		return new EqualsBuilder().append(goStructGeneratorStrategies, rhs.goStructGeneratorStrategies)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(goStructGeneratorStrategies)
		                                                                .toString();
	}
}
