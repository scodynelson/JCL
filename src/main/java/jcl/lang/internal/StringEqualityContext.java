package jcl.lang.internal;

import jcl.lang.IntegerStruct;
import jcl.lang.StringStruct;

/**
 * Context argument class for string functions that require equality checking arguments to produce the intended
 * result.
 */
public final class StringEqualityContext {

	/**
	 * Builder factory for {@link StringEqualityContext}.
	 */
	public static final class Builder {

		/**
		 * The {@link StringStruct} to compare against.
		 */
		private final StringStruct struct;

		/**
		 * The {@link StringIntervalOpContext.Builder} to build the intervals for the first {@link StringStruct} in
		 * the equality operation.
		 */
		private final StringIntervalOpContext.Builder intervalOpContext1 = StringIntervalOpContext.builder();

		/**
		 * The {@link StringIntervalOpContext.Builder} to build the intervals for the second {@link StringStruct} in
		 * the equality operation.
		 */
		private final StringIntervalOpContext.Builder intervalOpContext2 = StringIntervalOpContext.builder();

		/**
		 * Private constructor.
		 *
		 * @param struct
		 * 		the {@link StringStruct} to compare against
		 */
		private Builder(final StringStruct struct) {
			this.struct = struct;
		}

		/**
		 * Builder method for setting {@link StringIntervalOpContext.Builder#start} value for {@link
		 * #intervalOpContext1}.
		 *
		 * @param start1
		 * 		new value for {@link StringIntervalOpContext.Builder#start} for {@link #intervalOpContext1}
		 *
		 * @return the current builder instance
		 */
		public Builder start1(final IntegerStruct start1) {
			intervalOpContext1.start(start1);
			return this;
		}

		/**
		 * Builder method for setting {@link StringIntervalOpContext.Builder#end} value for {@link
		 * #intervalOpContext1}.
		 *
		 * @param end1
		 * 		new value for {@link StringIntervalOpContext.Builder#end} for {@link #intervalOpContext1}
		 *
		 * @return the current builder instance
		 */
		public Builder end1(final IntegerStruct end1) {
			intervalOpContext1.end(end1);
			return this;
		}

		/**
		 * Builder method for setting {@link StringIntervalOpContext.Builder#start} value for {@link
		 * #intervalOpContext2}.
		 *
		 * @param start2
		 * 		new value for {@link StringIntervalOpContext.Builder#start} for {@link #intervalOpContext1}
		 *
		 * @return the current builder instance
		 */
		public Builder start2(final IntegerStruct start2) {
			intervalOpContext2.start(start2);
			return this;
		}

		/**
		 * Builder method for setting {@link StringIntervalOpContext.Builder#end} value for {@link
		 * #intervalOpContext2}.
		 *
		 * @param end2
		 * 		new value for {@link StringIntervalOpContext.Builder#end} for {@link #intervalOpContext2}
		 *
		 * @return the current builder instance
		 */
		public Builder end2(final IntegerStruct end2) {
			intervalOpContext2.end(end2);
			return this;
		}

		/**
		 * Builder method for constructing a new {@link StringEqualityContext} from the current {@link #struct},
		 * {@link #intervalOpContext1}, and {@link #intervalOpContext2} values.
		 *
		 * @return a new {@link StringIntervalOpContext} from the current {@link #struct}, {@link
		 * #intervalOpContext1}, and {@link #intervalOpContext2} values
		 */
		public StringEqualityContext build() {
			return new StringEqualityContext(struct,
			                                 intervalOpContext1.build(),
			                                 intervalOpContext2.build());
		}
	}

	/**
	 * The {@link StringStruct} to compare against.
	 */
	private final StringStruct struct;

	/**
	 * The {@link StringIntervalOpContext} containing the interval boundaries for the first {@link StringStruct} in
	 * the equality operation.
	 */
	private final StringIntervalOpContext context1;

	/**
	 * The {@link StringIntervalOpContext} containing the interval boundaries for the second {@link StringStruct} in
	 * the equality operation.
	 */
	private final StringIntervalOpContext context2;

	/**
	 * Private constructor for initializing the {@link #struct}, {@link #context1}, and {@link #context2} values.
	 *
	 * @param struct
	 * 		the {@link StringStruct} to compare against
	 * @param context1
	 * 		{@link StringIntervalOpContext} containing the interval boundaries for the first {@link StringStruct} in
	 * 		the equality operation
	 * @param context2
	 * 		{@link StringIntervalOpContext} containing the interval boundaries for the second {@link StringStruct}
	 * 		in the equality operation
	 */
	private StringEqualityContext(final StringStruct struct,
	                              final StringIntervalOpContext context1,
	                              final StringIntervalOpContext context2) {
		this.struct = struct;
		this.context1 = context1;
		this.context2 = context2;
	}

	/**
	 * Getter for {@link #struct} value.
	 *
	 * @return {@link #struct} value
	 */
	public StringStruct getStruct() {
		return struct;
	}

	/**
	 * Getter for {@link #context1} value.
	 *
	 * @return {@link #context1} value
	 */
	public StringIntervalOpContext getContext1() {
		return context1;
	}

	/**
	 * Getter for {@link #context2} value.
	 *
	 * @return {@link #context2} value
	 */
	public StringIntervalOpContext getContext2() {
		return context2;
	}

	/**
	 * Factory method for retrieving a new {@link  Builder} object.
	 *
	 * @param struct
	 * 		the {@link StringStruct} to compare against
	 *
	 * @return a new {@link  Builder} object
	 */
	public static Builder builder(final StringStruct struct) {
		return new Builder(struct);
	}
}
