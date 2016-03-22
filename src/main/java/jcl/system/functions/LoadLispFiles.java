package jcl.system.functions;

import java.util.List;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.functions.CompileForm;
import jcl.compiler.functions.LoadFunction;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LoadLispFiles extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOAD-LISP-FILES";

	@Autowired
	private LoadFunction loadFunction;

	@Resource
	private List<String> lispFilesToLoad;

	public LoadLispFiles() {
		super("",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		CompileForm.OUTPUT_FILE = false;
		for (final String lispFileToLoad : lispFilesToLoad) {
			final PathnameStruct pathname = new PathnameStruct(lispFileToLoad);
			loadFunction.load(pathname, false, false, true);
		}
		CompileForm.OUTPUT_FILE = true;
		return TStruct.INSTANCE;
	}
}
