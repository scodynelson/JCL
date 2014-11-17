package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link Class} represents objects that determine the structure and behavior of their instances. Associated
 * with an object of type {@link Class} is information describing its place in the directed acyclic graph of classes,
 * its slots, and its options.
 * <p>
 * {@link Class} -> {@link StandardObject} -> {@link T}
 */
public interface Class extends StandardObject {

	Class INSTANCE = new Factory.ClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Class> {

		@Override
		public Class getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Class} type implementation.
		 */
		private static final class ClassImpl extends TypeBaseClass implements Class, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ClassImpl() {
				super("CLASS", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Class);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
