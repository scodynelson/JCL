package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.lambdalist.AuxBinding;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.lambdalist.RestBinding;
import jcl.compiler.real.environment.lambdalist.SuppliedPBinding;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LambdaListParser {

	private static final SymbolStruct<?> AND_OPTIONAL = GlobalPackageStruct.KEYWORD.intern("&OPTIONAL").getSymbolStruct();
	private static final SymbolStruct<?> AND_REST = GlobalPackageStruct.KEYWORD.intern("&REST").getSymbolStruct();
	private static final SymbolStruct<?> AND_KEY = GlobalPackageStruct.KEYWORD.intern("&KEY").getSymbolStruct();
	private static final SymbolStruct<?> AND_ALLOW_OTHER_KEYS = GlobalPackageStruct.KEYWORD.intern("&ALLOW-OTHER-KEYS").getSymbolStruct();
	private static final SymbolStruct<?> AND_AUX = GlobalPackageStruct.KEYWORD.intern("&AUX").getSymbolStruct();

	private static final SymbolStruct<?> AND_WHOLE = GlobalPackageStruct.KEYWORD.intern("&WHOLE").getSymbolStruct();
	private static final SymbolStruct<?> AND_BODY = GlobalPackageStruct.KEYWORD.intern("&BODY").getSymbolStruct();
	private static final SymbolStruct<?> AND_ENVIRONMENT = GlobalPackageStruct.KEYWORD.intern("&ENVIRONMENT").getSymbolStruct();

	public static OrdinaryLambdaListBindings parseOrdinaryLambdaList(final ListStruct lambdaList) {

		final List<LispStruct> lambdaListJava = lambdaList.getAsJavaList();
		final Iterator<LispStruct> iterator = lambdaListJava.iterator();

		int position = 0;

		final RequiredParseResult requiredParseResult = parseRequiredBindings(iterator, position);

		final List<RequiredBinding> requiredBindings = requiredParseResult.getRequiredBindings();
		LispStruct currentElement = requiredParseResult.getCurrentElement();
		position = requiredParseResult.getCurrentPosition();

		List<OptionalBinding> optionalBindings = null;
		if (currentElement.equals(AND_OPTIONAL)) {
			final OptionalParseResult optionalParseResult = parseOptionalBindings(iterator, position);

			optionalBindings = optionalParseResult.getOptionalBindings();
			currentElement = optionalParseResult.getCurrentElement();
			position = optionalParseResult.getCurrentPosition();
		}

		RestBinding restBinding = null;
		if (currentElement.equals(AND_REST)) {
			final RestParseResult restParseResult = parseRestBinding(iterator, position);

			restBinding = restParseResult.getRestBinding();
			currentElement = restParseResult.getCurrentElement();
			position = restParseResult.getCurrentPosition();
		}

		List<KeyBinding> keyBindings = null;
		boolean allowOtherKeys = false;
		if (currentElement.equals(AND_KEY)) {
			final KeyParseResult keyParseResult = parseKeyBindings(iterator, position);

			keyBindings = keyParseResult.getKeyBindings();
			allowOtherKeys = keyParseResult.isAllowOtherKeys();
			currentElement = keyParseResult.getCurrentElement();
			position = keyParseResult.getCurrentPosition();
		}

		List<AuxBinding> auxBindings = null;
		if (currentElement.equals(AND_AUX)) {
			final AuxParseResult auxParseResult = parseAuxBindings(iterator, position);

			auxBindings = auxParseResult.getAuxBindings();
		}

		if (iterator.hasNext()) {
			throw new RuntimeException("Unexpected element at the end of Ordinary Lambda List: " + iterator.next());
		}

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	/*
	 * BINDING PARSE METHODS
	 */

	private static RequiredParseResult parseRequiredBindings(final Iterator<LispStruct> iterator, final int position) {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (!(currentElement instanceof SymbolStruct)) {
				throw new RuntimeException("LambdaList required parameters must be of type SymbolStruct: " + currentElement);
			}
			final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
			final RequiredBinding requiredBinding = new RequiredBinding(currentParam, currentPosition++);
			requiredBindings.add(requiredBinding);

			currentElement = iterator.next();
		}

		return new RequiredParseResult(currentElement, currentPosition, requiredBindings);
	}

	private static OptionalParseResult parseOptionalBindings(final Iterator<LispStruct> iterator, final int position) {

		final List<OptionalBinding> optionalBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final OptionalBinding optionalBinding = new OptionalBinding(currentParam, currentPosition++, null, null);
				optionalBindings.add(optionalBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					throw new RuntimeException("LambdaList optional parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					throw new RuntimeException("LambdaList optional var name parameters must be of type SymbolStruct: " + firstInCurrent);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct<?>) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new RuntimeException("LambdaList optional supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct<?>) thirdInCurrent;
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, currentPosition++);
				}

				final OptionalBinding optionalBinding = new OptionalBinding(varNameCurrent, currentPosition++, initForm, suppliedPBinding);
				optionalBindings.add(optionalBinding);
			} else {
				throw new RuntimeException("LambdaList optional parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		return new OptionalParseResult(currentElement, currentPosition, optionalBindings);
	}

	private static RestParseResult parseRestBinding(final Iterator<LispStruct> iterator, final int position) {

		int currentPosition = position;

		final LispStruct currentElement = iterator.next();
		if (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			throw new RuntimeException("LambdaList rest parameter must only have 1 parameter: " + iterator.next());
		}

		if (!(currentElement instanceof SymbolStruct)) {
			throw new RuntimeException("LambdaList rest parameters must be of type SymbolStruct: " + currentElement);
		}
		final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;

		final RestBinding restBinding = new RestBinding(currentParam, currentPosition++);
		return new RestParseResult(currentElement, currentPosition, restBinding);
	}

	private static KeyParseResult parseKeyBindings(final Iterator<LispStruct> iterator, final int position) {

		final List<KeyBinding> keyBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final KeywordSymbolStruct keyName = GlobalPackageStruct.KEYWORD.intern(currentParam.getName()).getPackageSymbolType();
				final KeyBinding keyBinding = new KeyBinding(currentParam, currentPosition++, null, keyName, null);
				keyBindings.add(keyBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 3)) {
					throw new RuntimeException("LambdaList key parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();
				final LispStruct thirdInCurrent = currentParam.getRest().getRest().getFirst();

				final SymbolStruct<?> varNameCurrent;
				final KeywordSymbolStruct varKeyNameCurrent;
				if (firstInCurrent instanceof SymbolStruct) {
					varNameCurrent = (SymbolStruct<?>) firstInCurrent;
					varKeyNameCurrent = GlobalPackageStruct.KEYWORD.intern(varNameCurrent.getName()).getPackageSymbolType();
				} else if (firstInCurrent instanceof ListStruct) {
					final ListStruct currentVar = (ListStruct) firstInCurrent;
					if (currentVar.size() != 2) {
						throw new RuntimeException("LambdaList key var name list parameters must have 2 parameters: " + currentVar);
					}

					final LispStruct firstInCurrentVar = currentVar.getFirst();
					if (!(firstInCurrentVar instanceof KeywordSymbolStruct)) {
						throw new RuntimeException("LambdaList key var name list key-name parameters must be of type KeywordStruct: " + firstInCurrentVar);
					}
					varKeyNameCurrent = (KeywordSymbolStruct) firstInCurrentVar;

					final LispStruct secondInCurrentVar = currentVar.getRest().getFirst();
					if (!(secondInCurrentVar instanceof SymbolStruct)) {
						throw new RuntimeException("LambdaList key var name list name parameters must be of type SymbolStruct: " + secondInCurrentVar);
					}
					varNameCurrent = (SymbolStruct<?>) secondInCurrentVar;
				} else {
					throw new RuntimeException("LambdaList key var name parameters must be of type SymbolStruct or ListStruct: " + firstInCurrent);
				}

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				SuppliedPBinding suppliedPBinding = null;
				if (!thirdInCurrent.equals(NullStruct.INSTANCE)) {
					if (!(thirdInCurrent instanceof SymbolStruct)) {
						throw new RuntimeException("LambdaList key supplied-p parameters must be of type SymbolStruct: " + thirdInCurrent);
					}

					final SymbolStruct<?> suppliedPCurrent = (SymbolStruct<?>) thirdInCurrent;
					suppliedPBinding = new SuppliedPBinding(suppliedPCurrent, currentPosition++);
				}

				final KeyBinding keyBinding = new KeyBinding(varNameCurrent, currentPosition++, initForm, varKeyNameCurrent, suppliedPBinding);
				keyBindings.add(keyBinding);
			} else {
				throw new RuntimeException("LambdaList key parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		boolean allowOtherKeys = false;
		if (currentElement.equals(AND_ALLOW_OTHER_KEYS)) {
			allowOtherKeys = true;
			currentElement = iterator.next();
		}

		return new KeyParseResult(currentElement, currentPosition, keyBindings, allowOtherKeys);
	}

	private static AuxParseResult parseAuxBindings(final Iterator<LispStruct> iterator, final int position) {

		final List<AuxBinding> auxBindings = new ArrayList<>();
		int currentPosition = position;

		LispStruct currentElement = iterator.next();
		while (iterator.hasNext() && !isLambdaListKeyword(currentElement)) {
			if (currentElement instanceof SymbolStruct) {
				final SymbolStruct<?> currentParam = (SymbolStruct<?>) currentElement;
				final AuxBinding auxBinding = new AuxBinding(currentParam, currentPosition++, null);
				auxBindings.add(auxBinding);
			} else if (currentElement instanceof ListStruct) {
				final ListStruct currentParam = (ListStruct) currentElement;
				if ((currentParam.size() < 1) || (currentParam.size() > 2)) {
					throw new RuntimeException("LambdaList aux parameters must have between 1 and 3 parameters: " + currentParam);
				}

				final LispStruct firstInCurrent = currentParam.getFirst();
				final LispStruct secondInCurrent = currentParam.getRest().getFirst();

				if (!(firstInCurrent instanceof SymbolStruct)) {
					throw new RuntimeException("LambdaList aux var name parameters must be of type SymbolStruct: " + firstInCurrent);
				}
				final SymbolStruct<?> varNameCurrent = (SymbolStruct<?>) firstInCurrent;

				LispStruct initForm = null;
				if (!secondInCurrent.equals(NullStruct.INSTANCE)) {
					initForm = secondInCurrent;
				}

				final AuxBinding auxBinding = new AuxBinding(varNameCurrent, currentPosition++, initForm);
				auxBindings.add(auxBinding);
			} else {
				throw new RuntimeException("LambdaList aux parameters must be of type SymbolStruct or ListStruct: " + currentElement);
			}

			currentElement = iterator.next();
		}

		return new AuxParseResult(currentElement, currentPosition, auxBindings);
	}

	/*
	 * BINDING PARSE RESULT OBJECTS
	 */

	private abstract static class ParseResult {

		private final LispStruct currentElement;
		private final int currentPosition;

		protected ParseResult(final LispStruct currentElement, final int currentPosition) {
			this.currentElement = currentElement;
			this.currentPosition = currentPosition;
		}

		public LispStruct getCurrentElement() {
			return currentElement;
		}

		public int getCurrentPosition() {
			return currentPosition;
		}
	}

	private static class RequiredParseResult extends ParseResult {

		private final List<RequiredBinding> requiredBindings;

		private RequiredParseResult(final LispStruct currentElement, final int currentPosition, final List<RequiredBinding> requiredBindings) {
			super(currentElement, currentPosition);
			this.requiredBindings = requiredBindings;
		}

		public List<RequiredBinding> getRequiredBindings() {
			return requiredBindings;
		}
	}

	private static class OptionalParseResult extends ParseResult {

		private final List<OptionalBinding> optionalBindings;

		private OptionalParseResult(final LispStruct currentElement, final int currentPosition, final List<OptionalBinding> optionalBindings) {
			super(currentElement, currentPosition);
			this.optionalBindings = optionalBindings;
		}

		public List<OptionalBinding> getOptionalBindings() {
			return optionalBindings;
		}
	}

	private static class RestParseResult extends ParseResult {

		private final RestBinding restBinding;

		private RestParseResult(final LispStruct currentElement, final int currentPosition, final RestBinding restBinding) {
			super(currentElement, currentPosition);
			this.restBinding = restBinding;
		}

		public RestBinding getRestBinding() {
			return restBinding;
		}
	}

	private static class KeyParseResult extends ParseResult {

		private final List<KeyBinding> keyBindings;
		private final boolean allowOtherKeys;

		private KeyParseResult(final LispStruct currentElement, final int currentPosition, final List<KeyBinding> keyBindings,
							   final boolean allowOtherKeys) {
			super(currentElement, currentPosition);
			this.keyBindings = keyBindings;
			this.allowOtherKeys = allowOtherKeys;
		}

		public List<KeyBinding> getKeyBindings() {
			return keyBindings;
		}

		public boolean isAllowOtherKeys() {
			return allowOtherKeys;
		}
	}

	private static class AuxParseResult extends ParseResult {

		private final List<AuxBinding> auxBindings;

		private AuxParseResult(final LispStruct currentElement, final int currentPosition, final List<AuxBinding> auxBindings) {
			super(currentElement, currentPosition);
			this.auxBindings = auxBindings;
		}

		public List<AuxBinding> getAuxBindings() {
			return auxBindings;
		}
	}

	/*
	 * UTILITY METHODS
	 */

	private static boolean isLambdaListKeyword(final LispStruct lispStruct) {
		return lispStruct.equals(AND_AUX)
				|| lispStruct.equals(AND_ALLOW_OTHER_KEYS)
				|| lispStruct.equals(AND_KEY)
				|| lispStruct.equals(AND_OPTIONAL)
				|| lispStruct.equals(AND_REST)
				|| lispStruct.equals(AND_WHOLE)
				|| lispStruct.equals(AND_ENVIRONMENT)
				|| lispStruct.equals(AND_BODY);
	}
}
