package jcl.types.functions;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.lambdalist.variable.Key;
import jcl.types.lambdalist.variable.Optional;
import jcl.types.lambdalist.variable.Rest;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.TypeSpecifier;
import jcl.types.typespecifiers.compound.ValuesTypeSpecifier;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.List;

/**
 * A {@code Function} is an object that represents code to be executed when an appropriate number of arguments is supplied.
 */
public interface Function extends T {

	Function INSTANCE = new Factory.FunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Function> {

		@Override
		public Function getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Function} type.
		 *
		 * @param typeSpecifiers      the required arguments
		 * @param optional            the optional arguments
		 * @param rest                the rest arguments
		 * @param key                 the key arguments
		 * @param valuesTypeSpecifier the values arguments
		 * @return the newly created compound {@code Function} type
		 */
		public static Function getInstance(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
										   final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
										   final ValuesTypeSpecifier valuesTypeSpecifier) {
			return new FunctionImpl(typeSpecifiers, optional, rest, key, valuesTypeSpecifier);
		}

		/**
		 * Inner {@code Function} type implementation.
		 */
		private static class FunctionImpl implements Function, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final List<TypeSpecifier> typeSpecifiers;
			private final Optional<TypeSpecifier> optional;
			private final Rest<TypeSpecifier> rest;
			private final Key<TypeSpecifier> key;

			private final ValuesTypeSpecifier valuesTypeSpecifier;

			/**
			 * Private constructor.
			 */
			private FunctionImpl() {
				typeSpecifiers = null;
				optional = null;
				rest = null;
				key = null;

				valuesTypeSpecifier = null;
			}

			/**
			 * Private constructor for compound {@code Cons} type.
			 *
			 * @param typeSpecifiers      the required arguments
			 * @param optional            the optional arguments
			 * @param rest                the rest arguments
			 * @param key                 the key arguments
			 * @param valuesTypeSpecifier the values arguments
			 */
			private FunctionImpl(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
								 final Rest<TypeSpecifier> rest, final Key<TypeSpecifier> key,
								 final ValuesTypeSpecifier valuesTypeSpecifier) {
				this.typeSpecifiers = typeSpecifiers;
				this.optional = optional;
				this.rest = rest;
				this.key = key;

				this.valuesTypeSpecifier = valuesTypeSpecifier;
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Function)) {
					return false;
				}

				final Function function = (Function) obj;
				if (function == INSTANCE) {
					return true;
				}

				if (function instanceof FunctionImpl) {
					final FunctionImpl functionImpl = (FunctionImpl) function;

					// TODO: account for required/optional/etc
					return ObjectUtils.equals(typeSpecifiers, functionImpl.typeSpecifiers)
							&& ObjectUtils.equals(optional, functionImpl.optional)
							&& ObjectUtils.equals(rest, functionImpl.rest)
							&& ObjectUtils.equals(key, functionImpl.key)
							&& ObjectUtils.equals(valuesTypeSpecifier, functionImpl.valuesTypeSpecifier);
				}

				return false;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(typeSpecifiers)
						.append(optional)
						.append(rest)
						.append(key)
						.append(valuesTypeSpecifier)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "FunctionImpl{" +
						"typeSpecifiers=" + typeSpecifiers +
						", optional=" + optional +
						", rest=" + rest +
						", key=" + key +
						", valuesTypeSpecifier=" + valuesTypeSpecifier +
						'}';
			}
		}
	}
}
