package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.parameterdsl.FunctionParameters;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class TEMPMakeListFunction extends BuiltInFunctionStruct {

	public TEMPMakeListFunction() {
		super("Returns a list of length given by size, each of the elements of which is initial-element.", "TEMP-MAKE-LIST");
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
	public LispStruct apply(final FunctionParameters params) {
		final IntegerStruct size = (IntegerStruct) params.getRequiredParameters().get("size");
		final LispStruct initialElement = params.getKeyParameters().get(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD);
		return ListStruct.makeList(size.getBigInteger().longValue(), initialElement);
	}

	@Override
	protected FunctionParameters getParams(final List<LispStruct> lispStructs) {
		return Parameters.forFunction("TEMP-MAKE-LIST")
		                 .requiredParameter("size").as(IntegerStruct.class)
		                 .keyParameter(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD).as(LispStruct.class)
		                 .build(lispStructs);
	}
}
