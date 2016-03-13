package jcl.lists.functions;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeListFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public MakeListFunction() {
		super("Returns a list of length given by size, each of the elements of which is initial-element.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SIZE").buildList();
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		return KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "INITIAL-ELEMENT")
		                   .suppliedPBinding()
		                   .initForm(NILStruct.INSTANCE)
		                   .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final IntegerStruct sizeInteger
				= validator.validateType(lispStructs[0], functionName(), "Size", IntegerType.INSTANCE, IntegerStruct.class);
		final long size = sizeInteger.getBigInteger().longValue();

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 1, CommonLispSymbols.INITIAL_ELEMENT_KEYWORD);

		final LispStruct initialElement
				= keywords.getOrDefault(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD, NILStruct.INSTANCE);

		return ListStruct.makeList(size, initialElement);
	}

	@Override
	protected String functionName() {
		return "MAKE-LIST";
	}
}
