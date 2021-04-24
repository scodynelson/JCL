/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BignumStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.condition.exception.ParseErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link LogicalPathnameStructImpl} is the object representation of a Lisp 'logical-pathname' type.
 */
@Log4j2
public final class LogicalPathnameStructImpl extends PathnameStructImpl implements LogicalPathnameStruct {

	// A logical host is represented as the string that names it.
	// (defvar *logical-pathname-translations* (make-hash-table :test 'equal))
	private static final Map<String, LispStruct> LOGICAL_PATHNAME_TRANSLATIONS = new HashMap<>();

	/**
	 * Host marker character.
	 */
	private static final char HOST_MARKER = ':';

	/**
	 * Directory marker character.
	 */
	private static final char DIRECTORY_MARKER = ';';

	/**
	 * Type marker character.
	 */
	private static final char TYPE_MARKER = '.';

	private static final String LOGICAL_PATHNAME_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-;*.";

	private static final String NEWEST_UPPERCASE = "NEWEST";
	private static final String NEWEST_LOWERCASE = "newest";

	/**
	 * Public constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the logical-pathname object elements
	 */
	public LogicalPathnameStructImpl(final String pathname) {

		// Check for a logical pathname host.
		final String hostName = getHostString(pathname);
		String rest;
		if (hostName != null) {
			if (hostName.isEmpty()) {
				// "The null string, "", is not a valid value for any
				// component of a logical pathname." 19.3.2.2
				throw new ErrorException("Invalid logical host name: \"" + hostName + '"');
			}
			if (LOGICAL_PATHNAME_TRANSLATIONS.containsKey(hostName)) {
				// A defined logical pathname host.
				rest = pathname.substring(pathname.indexOf(HOST_MARKER) + 1);
			} else {
				throw new TypeErrorException("Logical namestring does not specify a host: \"" + pathname + '"');
			}
		} else {
			throw new TypeErrorException("Logical namestring does not specify a host: \"" + pathname + '"');
		}

		final int limit = rest.length();
		for (int i = 0; i < limit; i++) {
			final char c = rest.charAt(i);
			if (LOGICAL_PATHNAME_CHARS.indexOf(c) < 0) {
				throw new ParseErrorException("The character #\\" + c + " is not valid in a logical pathname.");
			}
		}

		host = StringStruct.toLispString(hostName);

		// "The device component of a logical pathname is always :UNSPECIFIC;
		// no other component of a logical pathname can be :UNSPECIFIC."
		device = CommonLispSymbols.UNSPECIFIC_KEYWORD;

		final int semi = rest.lastIndexOf(DIRECTORY_MARKER);
		if (semi >= 0) {
			// Directory.
			final String d = rest.substring(0, semi + 1);
			directory = parseDirectory(d);
			rest = rest.substring(semi + 1);
		} else {
			// "If a relative-directory-marker precedes the directories, the directory component parsed is as relative;
			// otherwise, the directory component is parsed as absolute."
			directory = ListStruct.toLispList(CommonLispSymbols.ABSOLUTE_KEYWORD);
		}

		int dot = rest.indexOf(TYPE_MARKER);
		if (dot >= 0) {
			final String n = rest.substring(0, dot);
			if (WILDCARD_STRING.equals(n)) {
				name = CommonLispSymbols.WILD_KEYWORD;
			} else {
				name = StringStruct.toLispString(n.toUpperCase());
			}
			rest = rest.substring(dot + 1);
			dot = rest.indexOf('.');
			if (dot >= 0) {
				final String t = rest.substring(0, dot);
				if (WILDCARD_STRING.equals(t)) {
					type = CommonLispSymbols.WILD_KEYWORD;
				} else {
					type = StringStruct.toLispString(t.toUpperCase());
				}
				// What's left is the version.
				final String v = rest.substring(dot + 1);
				if (WILDCARD_STRING.equals(v)) {
					version = CommonLispSymbols.WILD_KEYWORD;
				} else if (v.equals(NEWEST_UPPERCASE) || v.equals(NEWEST_LOWERCASE)) {
					version = CommonLispSymbols.NEWEST_KEYWORD;
				} else {
					version = CommonLispSymbols.PARSE_INTEGER.getFunction().apply(StringStruct.toLispString(v)); // TODO
				}
			} else {
				if (WILDCARD_STRING.equals(rest)) {
					type = CommonLispSymbols.WILD_KEYWORD;
				} else {
					type = StringStruct.toLispString(rest.toUpperCase());
				}
			}
		} else {
			if (WILDCARD_STRING.equals(rest)) {
				name = CommonLispSymbols.WILD_KEYWORD;
			} else if (!rest.isEmpty()) {
				name = StringStruct.toLispString(rest.toUpperCase());
			}
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param host
	 * 		the logical-pathname host
	 * @param directory
	 * 		the logical-pathname directory
	 * @param name
	 * 		the logical-pathname name
	 * @param type
	 * 		the logical-pathname type
	 * @param version
	 * 		the logical-pathname version
	 */
	public LogicalPathnameStructImpl(final LispStruct host, final LispStruct directory,
	                                 final LispStruct name, final LispStruct type, final LispStruct version) {
		super(host, CommonLispSymbols.UNSPECIFIC_KEYWORD, directory, name, type, version);
	}

	@Override
	public String namestring() {
		if (namestring != null) {
			return namestring;
		}
		if ((name == NILStruct.INSTANCE) && (type != NILStruct.INSTANCE)) {
			if (namestring != null) {
				throw new ErrorException("not null namestring??"); // TODO
			}
			return null;
		}
		if (directory instanceof StringStruct) {
			throw new ErrorException("bad directory??: " + directory); // TODO
		}
		final StringBuilder sb = new StringBuilder();
		// "If a pathname is converted to a namestring, the symbols NIL and
		// :UNSPECIFIC cause the field to be treated as if it were empty. That
		// is, both NIL and :UNSPECIFIC cause the component not to appear in
		// the namestring." 19.2.2.2.3.1
		if (host != NILStruct.INSTANCE) {
			if (!(host instanceof StringStruct)) {
//				Debug.assertTrue(host1 instanceof StringStruct || isURL());
				throw new ErrorException("bad host??: " + host); // TODO
			}

			sb.append(host);
			sb.append(':');
		}

		final String directoryNamestring = getDirectoryNamestring();

		sb.append(directoryNamestring);

		if (name instanceof StringStruct) {
			final String n = ((StringStruct) name).toJavaString();
			if (n.indexOf('/') >= 0) {
				if (namestring != null) {
					throw new ErrorException("not null namestring??"); // TODO
				}
				return null;
			}
			sb.append(n);
		} else if (name == CommonLispSymbols.WILD_KEYWORD) {
			sb.append('*');
		}
		if ((type != NILStruct.INSTANCE) && (type != CommonLispSymbols.UNSPECIFIC_KEYWORD)) {
			sb.append('.');
			if (type instanceof StringStruct) {
				final String t = ((StringStruct) type).toJavaString();
				// Allow Windows shortcuts to include TYPE
				if (!(t.endsWith(".lnk") && SystemUtils.IS_OS_WINDOWS)) {
					if (t.indexOf('.') >= 0) {
						if (namestring != null) {
							throw new ErrorException("not null namestring??"); // TODO
						}
						return null;
					}
				}
				sb.append(t);
			} else if (type == CommonLispSymbols.WILD_KEYWORD) {
				sb.append('*');
			} else {
				throw new ErrorException("Bad type??: " + type); // TODO
			}
		}

		if (version instanceof IntegerStruct) {
			sb.append('.');
			final int base = CommonLispSymbols.PRINT_BASE_VAR.getVariableValue().toJavaInt();
			if (version instanceof FixnumStruct) {
				sb.append(Integer.toString(((FixnumStruct) version).toJavaInteger(), base).toUpperCase());
			} else if (version instanceof BignumStruct) {
				sb.append(((BignumStruct) version).toJavaBigInteger().toString(base).toUpperCase());
			}
		} else if (version == CommonLispSymbols.WILD_KEYWORD) {
			sb.append(".*");
		} else if (version == CommonLispSymbols.NEWEST_KEYWORD) {
			sb.append(".NEWEST");
		}

		namestring = sb.toString();
		// XXX Decide if this is necessary
		// if (isURL()) {
		//     namestring = Utilities.uriEncode(namestring);
		// }
		return namestring;
	}

	@Override
	protected String getDirectoryNamestring() {
		final StringBuilder sb = new StringBuilder();
		// "If a pathname is converted to a namestring, the symbols NIL and
		// :UNSPECIFIC cause the field to be treated as if it were empty. That
		// is, both NIL and :UNSPECIFIC cause the component not to appear in
		// the namestring." 19.2.2.2.3.1
		if (directory != NILStruct.INSTANCE) {
			if (directory instanceof ListStruct) {
				ListStruct temp = (ListStruct) directory;
				LispStruct part = temp.car();
				if (part == CommonLispSymbols.ABSOLUTE_KEYWORD) {
				} else if (part == CommonLispSymbols.RELATIVE_KEYWORD) {
					sb.append(DIRECTORY_MARKER);
				} else {
//				throw new FileErrorException("Unsupported directory component " + part + '.', this); // TODO: send 'this' into error??
					throw new FileErrorException("Unsupported directory component " + part + '.', null);
				}
				temp = (ListStruct) temp.cdr();
				while (temp != NILStruct.INSTANCE) {
					part = temp.car();
					if (part instanceof StringStruct) {
						sb.append(((StringStruct) part).toJavaString());
					} else if (part == CommonLispSymbols.WILD_KEYWORD) {
						sb.append(WILDCARD_STRING);
					} else if (part == CommonLispSymbols.WILD_INFERIORS_KEYWORD) {
						sb.append(WILDCARD_INFERIORS_STRING);
					} else if (part == CommonLispSymbols.UP_KEYWORD) {
						sb.append(BACK_UP_STRING);
					} else {
//					throw new FileErrorException("Unsupported directory component " + part + '.', this); // TODO: send 'this' into error??
						throw new FileErrorException("Unsupported directory component " + part + '.', null);
					}
					sb.append(DIRECTORY_MARKER);
					temp = (ListStruct) temp.cdr();
				}
			} else if (directory instanceof StringStruct) {
				sb.append(directory);
			}
		}
		return sb.toString();
	}

	// "one or more uppercase letters, digits, and hyphens"
	private static String getHostString(final String s) {
		final int colon = s.indexOf(HOST_MARKER);
		if (colon >= 0) {
			return s.substring(0, colon).toUpperCase();
		} else {
			return null;
		}
	}

	private static ListStruct parseDirectory(final String s) {
		String directoryString = s;

		ListStruct result;
		if (directoryString.charAt(0) == DIRECTORY_MARKER) {
			result = ListStruct.toLispList(CommonLispSymbols.RELATIVE_KEYWORD);
			directoryString = directoryString.substring(1);
		} else {
			result = ListStruct.toLispList(CommonLispSymbols.ABSOLUTE_KEYWORD);
		}

		final StringTokenizer st = new StringTokenizer(directoryString, String.valueOf(DIRECTORY_MARKER));
		while (st.hasMoreTokens()) {
			final String token = st.nextToken();
			final LispStruct obj;
			if (WILDCARD_STRING.equals(token)) {
				obj = CommonLispSymbols.WILD_INFERIORS_KEYWORD;
			} else if (WILDCARD_INFERIORS_STRING.equals(token)) {
				obj = CommonLispSymbols.WILD_INFERIORS_KEYWORD;
			} else if (BACK_UP_STRING.equals(token)) {
				if (result.car() instanceof StringStruct) {
					result = (ListStruct) result.cdr();
					continue;
				}
				obj= CommonLispSymbols.UP_KEYWORD;
			} else {
				obj = StringStruct.toLispString(token.toUpperCase());
			}
			result = ConsStruct.toLispCons(obj, result);
		}
		return result.nReverse();
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LogicalPathnameStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link LogicalPathnameStruct} value</li>
	 * <li>Constructing a new {@link LogicalPathnameStruct} with the built {@link #namestring} value</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		host.generate(generatorState);
		directory.generate(generatorState);
		name.generate(generatorState);
		type.generate(generatorState);
		version.generate(generatorState);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LOGICAL_PATHNAME_STRUCT_NAME,
		                   GenerationConstants.LOGICAL_PATHNAME_STRUCT_TO_LOGICAL_PATHNAME_METHOD_NAME,
		                   GenerationConstants.LOGICAL_PATHNAME_STRUCT_TO_LOGICAL_PATHNAME_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.LOGICAL_PATHNAME;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.LOGICAL_PATHNAME;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.LOGICAL_PATHNAME) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.LOGICAL_PATHNAME) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
