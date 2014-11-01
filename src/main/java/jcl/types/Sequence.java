package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Sequence} is an ordered collection of objects, called the elements of the sequence. The type {@link Vector}
 * and the type {@link List} are disjoint subtypes of type {@link Sequence}, but are not necessarily an exhaustive
 * partition of the {@link Sequence}. When viewing a {@link Vector} as a {@link Sequence}, only the active elements of
 * that {@link Vector} are considered elements of the {@link Sequence}; that is, {@link Sequence} operations respect
 * the fill pointer when the given {@link Sequence} represents a {@link Vector}.
 * <p>
 * {@link Sequence} -> {@link T}
 */
public interface Sequence extends T {

	Sequence INSTANCE = new Factory.SequenceImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Sequence> {

		@Override
		public Sequence getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Sequence} type implementation.
		 */
		private static class SequenceImpl extends TypeBaseClass implements Sequence, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SequenceImpl() {
				super("SEQUENCE", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Sequence);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
