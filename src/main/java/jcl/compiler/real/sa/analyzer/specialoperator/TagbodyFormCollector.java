/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStructGenerator;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class TagbodyFormCollector implements Collector<LispStruct, Map<GoStruct<?>, PrognStruct>, Map<GoStruct<?>, PrognStruct>> {

	private final FormAnalyzer formAnalyzer;

	private final Environment environment;

	private final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies;

	private GoStruct<?> currentTag;

	TagbodyFormCollector(final FormAnalyzer formAnalyzer, final Environment environment,
	                     final Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies) {
		this.formAnalyzer = formAnalyzer;
		this.environment = environment;
		this.goStructGeneratorStrategies = goStructGeneratorStrategies;
		currentTag = null;
	}

	@Override
	public Supplier<Map<GoStruct<?>, PrognStruct>> supplier() {
		return HashMap::new;
	}

	@Override
	public BiConsumer<Map<GoStruct<?>, PrognStruct>, LispStruct> accumulator() {
		return (tagToFormsMap, current) -> {

			final GoStructGenerator<LispStruct> goStructGenerator = goStructGeneratorStrategies.get(current.getClass());
			if (goStructGenerator == null) {
				handleOtherwise(tagToFormsMap, current);
			} else {
				currentTag = goStructGenerator.generateGoElement(current);
			}
		};
	}

	private void handleOtherwise(final Map<GoStruct<?>, PrognStruct> tagToFormsMap, final LispStruct lispStruct) {
		if (!tagToFormsMap.containsKey(currentTag)) {
			tagToFormsMap.put(currentTag, new PrognStruct(new ArrayList<>()));
		}

		final LispStruct analyzedForm = formAnalyzer.analyze(lispStruct, environment);
		tagToFormsMap.get(currentTag).getForms().add(analyzedForm);
	}

	@Override
	public BinaryOperator<Map<GoStruct<?>, PrognStruct>> combiner() {
		return (tagToFormsMap1, tagToFormsMap2) -> {
			for (Map.Entry<GoStruct<?>, PrognStruct> e : tagToFormsMap2.entrySet()) {
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(formAnalyzer)
		                            .append(environment)
		                            .append(goStructGeneratorStrategies)
		                            .append(currentTag)
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
		final TagbodyFormCollector rhs = (TagbodyFormCollector) obj;
		return new EqualsBuilder().append(formAnalyzer, rhs.formAnalyzer)
		                          .append(environment, rhs.environment)
		                          .append(goStructGeneratorStrategies, rhs.goStructGeneratorStrategies)
		                          .append(currentTag, rhs.currentTag)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(environment)
		                                                                .append(goStructGeneratorStrategies)
		                                                                .append(currentTag)
		                                                                .toString();
	}
}
