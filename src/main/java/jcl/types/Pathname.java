package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Pathname} is a structured object which represents a filename.
 * <p>
 * There are two kinds of {@link Pathname}s---physical pathnames and {@link LogicalPathname}s.
 * <p>
 * {@link Pathname} -> {@link T}
 */
public interface Pathname extends T {

	Pathname INSTANCE = new Factory.PathnameImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Pathname> {

		@Override
		public Pathname getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Pathname} type implementation.
		 */
		private static final class PathnameImpl extends TypeBaseClass implements Pathname, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private PathnameImpl() {
				super("PATHNAME");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Pathname);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
