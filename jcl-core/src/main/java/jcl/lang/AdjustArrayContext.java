package jcl.lang;

import java.util.Arrays;
import java.util.List;

import jcl.type.LispType;

public class AdjustArrayContext {

	public static class Builder {

		private final IntegerStruct[] dimensions;
		private LispType elementType;
		private LispStruct initialElement;
		private SequenceStruct initialContents;
		private BooleanStruct adjustable = NILStruct.INSTANCE;
		private IntegerStruct fillPointer;
		private ArrayStruct displacedTo;
		private IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		private Builder(final IntegerStruct... dimensions) {
			this.dimensions = dimensions;
		}

		public Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public Builder initialElement(final LispStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		public Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		public Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		public Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		public Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		public Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		public AdjustArrayContext build() {
			return new AdjustArrayContext(Arrays.asList(dimensions),
			                              elementType,
			                              initialElement,
			                              initialContents,
			                              adjustable,
			                              fillPointer,
			                              displacedTo,
			                              displacedIndexOffset);
		}
	}

	private final List<IntegerStruct> dimensions;
	private final LispType elementType;
	private final LispStruct initialElement;
	private final SequenceStruct initialContents;
	private final BooleanStruct adjustable;
	private final IntegerStruct fillPointer;
	private final ArrayStruct displacedTo;
	private final IntegerStruct displacedIndexOffset;

	private AdjustArrayContext(final List<IntegerStruct> dimensions,
	                           final LispType elementType,
	                           final LispStruct initialElement,
	                           final SequenceStruct initialContents,
	                           final BooleanStruct adjustable,
	                           final IntegerStruct fillPointer,
	                           final ArrayStruct displacedTo,
	                           final IntegerStruct displacedIndexOffset) {
		this.dimensions = dimensions;
		this.elementType = elementType;
		this.initialElement = initialElement;
		this.initialContents = initialContents;
		this.adjustable = adjustable;
		this.fillPointer = fillPointer;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	public List<IntegerStruct> getDimensions() {
		return dimensions;
	}

	public LispType getElementType() {
		return elementType;
	}

	public LispStruct getInitialElement() {
		return initialElement;
	}

	public SequenceStruct getInitialContents() {
		return initialContents;
	}

	public BooleanStruct getAdjustable() {
		return adjustable;
	}

	public IntegerStruct getFillPointer() {
		return fillPointer;
	}

	public ArrayStruct getDisplacedTo() {
		return displacedTo;
	}

	public IntegerStruct getDisplacedIndexOffset() {
		return displacedIndexOffset;
	}

	public static Builder builder(final IntegerStruct... dimensions) {
		return new Builder(dimensions);
	}
}
