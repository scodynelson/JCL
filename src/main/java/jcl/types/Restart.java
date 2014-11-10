package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Restart} represents a {@link Function} that can be called to perform some form of recovery action, usually
 * a transfer of control to an outer point in the running program.
 * <p>
 * A {@link Restart} has only dynamic extent relative to the scope of the binding form which establishes it.
 * <p>
 * {@link Restart} -> {@link T}
 */
public interface Restart extends T {

	Restart INSTANCE = new Factory.RestartImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Restart> {

		@Override
		public Restart getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Restart} type implementation.
		 */
		private static final class RestartImpl extends TypeBaseClass implements Restart, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private RestartImpl() {
				super("RESTART", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Restart);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
