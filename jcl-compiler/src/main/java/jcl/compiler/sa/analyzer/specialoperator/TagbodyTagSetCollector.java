/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import jcl.compiler.struct.specialoperator.go.GoIntegerStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.struct.specialoperator.go.GoSymbolStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

final class TagbodyTagSetCollector implements Collector<LispStruct, List<GoStruct<?>>, List<GoStruct<?>>> {

	@Override
	public Supplier<List<GoStruct<?>>> supplier() {
		// NOTE: use ArrayList so the tags are ordered appropriately
		return ArrayList::new;
	}

	@Override
	public BiConsumer<List<GoStruct<?>>, LispStruct> accumulator() {
		return (tagSet, current) -> {

			if (current instanceof SymbolStruct) {
				final GoStruct<?> goTag = new GoSymbolStruct((SymbolStruct) current);
				tagSet.add(goTag);
			} else if (current instanceof IntegerStruct) {
				final GoStruct<?> goTag = new GoIntegerStruct((IntegerStruct) current);
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
