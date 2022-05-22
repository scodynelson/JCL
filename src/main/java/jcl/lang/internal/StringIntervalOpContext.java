package jcl.lang.internal;

import jcl.lang.IntegerStruct;

/**
 * Context argument class for string functions that require interval arguments to produce the intended result.
 */
public final class StringIntervalOpContext {

	/**
	 * Builder factory for {@link StringIntervalOpContext}.
	 */
	public static final class Builder {

		/**
		 * Starting index to perform a string operation.
		 */
		private IntegerStruct start;

		/**
		 * Ending index to perform a string operation.
		 */
		private IntegerStruct end;

		/**
		 * Private constructor.
		 */
		private Builder() {
		}

		/**
		 * Builder method for setting {@link #start} value.
		 *
		 * @param start
		 * 		new value for {@link #start}
		 *
		 * @return the current builder instance
		 */
		public Builder start(final IntegerStruct start) {
			this.start = start;
			return this;
		}

		/**
		 * Builder method for setting {@link #end} value.
		 *
		 * @param end
		 * 		new value for {@link #end}
		 *
		 * @return the current builder instance
		 */
		public Builder end(final IntegerStruct end) {
			this.end = end;
			return this;
		}

		/**
		 * Builder method for constructing a new {@link StringIntervalOpContext} from the current {@link #start} and
		 * {@link #end} values.
		 *
		 * @return a new {@link StringIntervalOpContext} from the current {@link #start} and {@link #end} values
		 */
		public StringIntervalOpContext build() {
			return new StringIntervalOpContext(start, end);
		}
	}

	/**
	 * Starting index to perform a string operation.
	 */
	private final IntegerStruct start;

	/**
	 * Ending index to perform a string operation.
	 */
	private final IntegerStruct end;

	/**
	 * Private constructor initializing {@link #start} and {@link #end} values.
	 *
	 * @param start
	 * 		initial value for {@link #start}
	 * @param end
	 * 		initial value for {@link #end}
	 */
	private StringIntervalOpContext(final IntegerStruct start, final IntegerStruct end) {
		this.start = start;
		this.end = end;
	}

	/**
	 * Getter for {@link #start} value.
	 *
	 * @return {@link #start} value
	 */
	public IntegerStruct getStart() {
		return start;
	}

	/**
	 * Getter for {@link #end} value.
	 *
	 * @return {@link #end} value
	 */
	public IntegerStruct getEnd() {
		return end;
	}

	/**
	 * Factory method for retrieving a new {@link  Builder} object.
	 *
	 * @return a new {@link  Builder} object
	 */
	public static Builder builder() {
		return new Builder();
	}
}
