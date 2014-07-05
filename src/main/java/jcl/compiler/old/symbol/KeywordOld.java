package jcl.compiler.old.symbol;

import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;

public class KeywordOld {

	/**
	 * An instance of the Keyword Default
	 */
	public static final SymbolStruct Default = new KeywordSymbolStruct("DEFAULT");
	/**
	 * An instance of the Keyword Upcase
	 */
	public static final SymbolStruct Upcase = new KeywordSymbolStruct("UPCASE");
	/**
	 * An instance of the Keyword Internal
	 */
	public static final SymbolStruct Internal = new KeywordSymbolStruct("INTERNAL");
	/**
	 * An instance of the Keyword External
	 */
	public static final SymbolStruct External = new KeywordSymbolStruct("EXTERNAL");
	/**
	 * An instance of the Keyword Inherited
	 */
	public static final SymbolStruct Inherited = new KeywordSymbolStruct("INHERITED");
	/**
	 * An instance of the Keyword DownCase
	 */
	public static final SymbolStruct Downcase = new KeywordSymbolStruct("DOWNCASE");
	/**
	 * An instance of the Keyword Capitalize
	 */
	public static final SymbolStruct Capitalize = new KeywordSymbolStruct("CAPITALIZE");
	/**
	 * An instance of the Keyword Preserve
	 */
	public static final SymbolStruct Preserve = new KeywordSymbolStruct("PRESERVE");
	/**
	 * An instance of the Keyword Invert
	 */
	public static final SymbolStruct Invert = new KeywordSymbolStruct("INVERT");

	/* Keywords for compiler environment */
	public static final SymbolStruct LoadTimeValue = new KeywordSymbolStruct("LOAD-TIME-VALUE");
	/**
	 * An instance of the Keyword FIELD_NAME
	 */
	public static final SymbolStruct FieldName = new KeywordSymbolStruct("FIELD_NAME");
	/**
	 * An instance of the Keyword Binding
	 */
	public static final SymbolStruct Binding = new KeywordSymbolStruct("BINDING");
	/**
	 * An instance of the Keyword Bindings
	 */
	public static final SymbolStruct Bindings = new KeywordSymbolStruct("BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final SymbolStruct FletBinding = new KeywordSymbolStruct("FLET-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final SymbolStruct FletBindings = new KeywordSymbolStruct("FLET-BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final SymbolStruct LabelsBinding = new KeywordSymbolStruct("LABELS-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final SymbolStruct LabelsBindings = new KeywordSymbolStruct("LABELS-BINDINGS");
	/**
	 * An instance of the Keyword SymbolTable
	 */
	public static final SymbolStruct SymbolTable = new KeywordSymbolStruct("SYMBOL-TABLE");
	/**
	 * An instance of the Keyword Closure
	 */
	public static final SymbolStruct Closure = new KeywordSymbolStruct("CLOSURE");
	/**
	 * An instance of the Keyword Parent
	 */
	public static final SymbolStruct Parent = new KeywordSymbolStruct("PARENT");
	/**
	 * An instance of the Keyword Type
	 */
	public static final SymbolStruct Type = new KeywordSymbolStruct("TYPE");
	/**
	 * An instance of the Keyword Allocation
	 */
	public static final SymbolStruct Allocation = new KeywordSymbolStruct("ALLOCATION");
	/**
	 * An instance of the Keyword Scope
	 */
	public static final SymbolStruct Scope = new KeywordSymbolStruct("SCOPE");
	/**
	 * An instance of the Keyword InitForm
	 */
	public static final SymbolStruct InitForm = new KeywordSymbolStruct("INIT-FORM");
	/**
	 * An instance of the Keyword UnboundValue
	 */
	public static final SymbolStruct UnboundValue = new KeywordSymbolStruct("*UNBOUND-VALUE*");
	/**
	 * An instance of the Keyword Lexical
	 */
	public static final SymbolStruct Lexical = new KeywordSymbolStruct("LEXICAL");
	/**
	 * An instance of the Keyword Dynamic
	 */
	public static final SymbolStruct Dynamic = new KeywordSymbolStruct("DYNAMIC");
	/**
	 * An instance of the Keyword Parameter
	 */
	public static final SymbolStruct Parameter = new KeywordSymbolStruct("PARAMETER");
	/**
	 * An instance of the Keyword Local
	 */
	public static final SymbolStruct Local = new KeywordSymbolStruct("LOCAL");
	/**
	 * An instance of the Keyword References
	 */
	public static final SymbolStruct References = new KeywordSymbolStruct("REFERENCES");
	/**
	 * An instance of the Keyword Required
	 */
	public static final SymbolStruct Required = new KeywordSymbolStruct("REQUIRED");
	/**
	 * An instance of the Keyword Free
	 */
	public static final SymbolStruct Free = new KeywordSymbolStruct("FREE");
	/**
	 * An instance of the Keyword Position
	 */
	public static final SymbolStruct Position = new KeywordSymbolStruct("POSITION");
	/**
	 * An instance of the Keyword Depth
	 */
	public static final SymbolStruct Depth = new KeywordSymbolStruct("DEPTH");

	public static final SymbolStruct CompileToplevel = new KeywordSymbolStruct("COMPILE-TOPLEVEL");
	public static final SymbolStruct LoadToplevel = new KeywordSymbolStruct("LOAD-TOPLEVEL");
	public static final SymbolStruct Execute = new KeywordSymbolStruct("EXECUTE");

	/** Keywords for Pathnames
	 *  (NOTE:  An instance of the Keyword Type and Local are
	 *  already defined earlier in this file */
	/**
	 * An instance of the Keyword Absolute
	 */
	public static final SymbolStruct Absolute = new KeywordSymbolStruct("ABSOLUTE");
	/**
	 * An instance of the Keyword Relative
	 */
	public static final SymbolStruct Relative = new KeywordSymbolStruct("RELATIVE");
	/**
	 * An instance of the Keyword Wild
	 */
	public static final SymbolStruct Wild = new KeywordSymbolStruct("WILD");
	/**
	 * An instance of the Keyword Defaults
	 */
	public static final SymbolStruct Defaults = new KeywordSymbolStruct("DEFAULTS");
	/**
	 * An instance of the Keyword Unspecific
	 */
	public static final SymbolStruct Unspecific = new KeywordSymbolStruct("UNSPECIFIC");
	/**
	 * An instance of the Keyword Back
	 */
	public static final SymbolStruct Back = new KeywordSymbolStruct("BACK");
	/**
	 * An instance of the Keyword Case
	 */
	public static final SymbolStruct Case = new KeywordSymbolStruct("CASE");
	/**
	 * An instance of the Keyword Common
	 */
	public static final SymbolStruct Common = new KeywordSymbolStruct("COMMON");
	/**
	 * An instance of the Keyword Wild-inferior
	 */
	public static final SymbolStruct WildInferior = new KeywordSymbolStruct("WILD-INFERIOR");
	/**
	 * An instance of the Keyword Newest
	 */
	public static final SymbolStruct Newest = new KeywordSymbolStruct("NEWEST");
	/**
	 * An instance of the Keyword Oldest
	 */
	public static final SymbolStruct Oldest = new KeywordSymbolStruct("OLDEST");
	/**
	 * An instance of the Keyword Host.
	 */
	public static final SymbolStruct Host = new KeywordSymbolStruct("HOST");
	/**
	 * An instance of the Keyword Device.
	 */
	public static final SymbolStruct Device = new KeywordSymbolStruct("DEVICE");
	/**
	 * An instance of the Keyword Directory.
	 */
	public static final SymbolStruct Directory = new KeywordSymbolStruct("DIRECTORY");
	/**
	 * An instance of the Keyword Name.
	 */
	public static final SymbolStruct Name = new KeywordSymbolStruct("NAME");
	/**
	 * An instance of the Keyword Version.
	 */
	public static final SymbolStruct Version = new KeywordSymbolStruct("VERSION");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final SymbolStruct Start = new KeywordSymbolStruct("START");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final SymbolStruct End = new KeywordSymbolStruct("END");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final SymbolStruct JunkAllowed = new KeywordSymbolStruct("JUNK-ALLOWED");

	/** Keywords added for URI support (these are not a part of Common Lisp) */
	/**
	 * An instance of the Keyword News.
	 */
	public static final SymbolStruct News = new KeywordSymbolStruct("news");
	/**
	 * An instance of the Keyword File. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct File = new KeywordSymbolStruct("file");
	/**
	 * An instance of the Keyword HTTP. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct HTTP = new KeywordSymbolStruct("http");
	/**
	 * An instance of the Keyword MAILTO. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct MailTo = new KeywordSymbolStruct("mailto");
	/**
	 * An instance of the Keyword HTTPS. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct HTTPS = new KeywordSymbolStruct("https");
	/**
	 * An instance of the Keyword PORT. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct Port = new KeywordSymbolStruct("PORT");
	/**
	 * An instance of the Keyword USER-INFO. This is added for URI support and is not part of standard Lisp
	 */
	public static final SymbolStruct UserInfo = new KeywordSymbolStruct("USER-INFO");

	/** Keywords added for Open.java **/
	/**
	 * An instance of the Keyword Direction.
	 */
	public static final SymbolStruct Direction = new KeywordSymbolStruct("DIRECTION");
	/**
	 * An instance of the Keyword Input.
	 */
	public static final SymbolStruct Input = new KeywordSymbolStruct("INPUT");
	/**
	 * An instance of the Keyword Output.
	 */
	public static final SymbolStruct Output = new KeywordSymbolStruct("OUTPUT");
	/**
	 * An instance of the Keyword IO.
	 */
	public static final SymbolStruct IO = new KeywordSymbolStruct("IO");
	/**
	 * An instance of the Keyword Probe.
	 */
	public static final SymbolStruct Probe = new KeywordSymbolStruct("PROBE");
	/**
	 * An instance of the Keyword ElementType.
	 */
	public static final SymbolStruct ElementType = new KeywordSymbolStruct("ELEMENT-TYPE");
	/**
	 * An instance of the Keyword Character.
	 */
	public static final SymbolStruct Character = new KeywordSymbolStruct("CHARACTER");
	/**
	 * An instance of the Keyword UnsignedByte.
	 */
	public static final SymbolStruct UnsignedByte = new KeywordSymbolStruct("UNSIGNED-BYTE");
	/**
	 * An instance of the Keyword SignedByte.
	 */
	public static final SymbolStruct SignedByte = new KeywordSymbolStruct("SIGNED-BYTE");
	/**
	 * An instance of the Keyword IfExists.
	 */
	public static final SymbolStruct IfExists = new KeywordSymbolStruct("IF-EXISTS");
	/**
	 * An instance of the Keyword Error.
	 */
	public static final SymbolStruct Error = new KeywordSymbolStruct("ERROR");
	/**
	 * An instance of the Keyword NewVersion.
	 */
	public static final SymbolStruct NewVersion = new KeywordSymbolStruct("NEW-VERSION");
	/**
	 * An instance of the Keyword Rename.
	 */
	public static final SymbolStruct Rename = new KeywordSymbolStruct("RENAME");
	/**
	 * An instance of the Keyword RenameAndDelete.
	 */
	public static final SymbolStruct RenameAndDelete = new KeywordSymbolStruct("RENAME-AND-DELETE");
	/**
	 * An instance of the Keyword Overwrite.
	 */
	public static final SymbolStruct Overwrite = new KeywordSymbolStruct("OVERWRITE");
	/**
	 * An instance of the Keyword Append.
	 */
	public static final SymbolStruct Append = new KeywordSymbolStruct("APPEND");
	/**
	 * An instance of the Keyword Supersede.
	 */
	public static final SymbolStruct Supersede = new KeywordSymbolStruct("SUPERSEDE");
	/**
	 * An instance of the Keyword IfDoesNotExist.
	 */
	public static final SymbolStruct IfDoesNotExist = new KeywordSymbolStruct("IF-DOES-NOT-EXIST");
	/**
	 * An instance of the Keyword Create.
	 */
	public static final SymbolStruct Create = new KeywordSymbolStruct("CREATE");
	/**
	 * An instance of the Keyword ExternalFormat.
	 */
	public static final SymbolStruct ExternalFormat = new KeywordSymbolStruct("EXTERNAL-FORMAT");

}
