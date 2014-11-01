package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Readtable} maps characters into syntax types for the Lisp reader. A {@link Readtable} also contains
 * associations between macro characters and their reader macro functions, and records information about the case
 * conversion rules to be used by the Lisp reader when parsing {@link Symbol}s.
 * <p>
 * Each simple {@link Character} must be representable in the {@link Readtable}.
 * <p>
 * {@link Readtable} -> {@link T}
 */
public interface Readtable extends T {

	Readtable INSTANCE = new Factory.ReadtableImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Readtable> {

		@Override
		public Readtable getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Readtable} type implementation.
		 */
		private static class ReadtableImpl extends TypeBaseClass implements Readtable, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ReadtableImpl() {
				super("READTABLE", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Readtable);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
