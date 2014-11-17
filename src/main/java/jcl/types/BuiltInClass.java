package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link BuiltInClass} is a {@link Class} whose instances have restricted capabilities or special representations.
 * {@link BuiltInClass}es can be used as parameter specializers in {@link Method}s.
 * <p>
 * {@link BuiltInClass} -> {@link Class} -> {@link StandardObject} -> {@link T}
 */
public interface BuiltInClass extends Class {

	BuiltInClass INSTANCE = new Factory.BuiltInClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BuiltInClass> {

		@Override
		public BuiltInClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BuiltInClass} type implementation.
		 */
		private static final class BuiltInClassImpl extends TypeBaseClass implements BuiltInClass, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BuiltInClassImpl() {
				super("BUILT-IN-CLASS", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BuiltInClass);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
