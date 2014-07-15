package jcl.compiler.real.sa;

import jcl.compiler.real.environment.Binding;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class LambdaListParser {

	public static final KeywordSymbolStruct REST = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("REST").getSymbolStruct();
	public static final KeywordSymbolStruct WHOLE = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("WHOLE").getSymbolStruct();
	public static final KeywordSymbolStruct ENVIRONMENT = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("ENVIRONMENT").getSymbolStruct();
	public static final KeywordSymbolStruct REQUIRED = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("REQUIRED").getSymbolStruct();
	public static final KeywordSymbolStruct OPTIONAL = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("OPTIONAL").getSymbolStruct();
	public static final KeywordSymbolStruct SUPPLIED_P = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("SUPPLIED-P").getSymbolStruct();
	public static final KeywordSymbolStruct KEY = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("KEY").getSymbolStruct();
	public static final KeywordSymbolStruct BODY = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("BODY").getSymbolStruct();
	public static final KeywordSymbolStruct AUX = (KeywordSymbolStruct) GlobalPackageStruct.KEYWORD.intern("AUX").getSymbolStruct();

	private static final SymbolStruct<?> AND_REST = GlobalPackageStruct.KEYWORD.intern("&REST").getSymbolStruct();
	private static final SymbolStruct<?> AND_WHOLE = GlobalPackageStruct.KEYWORD.intern("&WHOLE").getSymbolStruct();
	private static final SymbolStruct<?> AND_ENVIRONMENT = GlobalPackageStruct.KEYWORD.intern("&ENVIRONMENT").getSymbolStruct();
	private static final SymbolStruct<?> AND_REQUIRED = GlobalPackageStruct.KEYWORD.intern("&REQUIRED").getSymbolStruct();
	private static final SymbolStruct<?> AND_OPTIONAL = GlobalPackageStruct.KEYWORD.intern("&OPTIONAL").getSymbolStruct();
	private static final SymbolStruct<?> AND_SUPPLIED_P = GlobalPackageStruct.KEYWORD.intern("&SUPPLIED-P").getSymbolStruct();
	private static final SymbolStruct<?> AND_KEY = GlobalPackageStruct.KEYWORD.intern("&KEY").getSymbolStruct();
	private static final SymbolStruct<?> AND_BODY = GlobalPackageStruct.KEYWORD.intern("&BODY").getSymbolStruct();
	private static final SymbolStruct<?> AND_AUX = GlobalPackageStruct.KEYWORD.intern("&AUX").getSymbolStruct();
	private static final SymbolStruct<?> AND_ALLOW_OTHER_KEYS = GlobalPackageStruct.KEYWORD.intern("&ALLOW-OTHER-KEYS").getSymbolStruct();

	public static List<Binding> parseOrdinaryLambdaList(final ListStruct lambdaList) {
		return null;
	}

}
