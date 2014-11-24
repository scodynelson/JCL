package jcl.compiler.old.functions;

import jcl.compiler.old.symbol.KeywordOld;
import jcl.arrays.StringStruct;
import jcl.lists.NullStruct;
import jcl.pathnames.PathnameStruct;
import jcl.streams.StreamStruct;

import java.io.File;
import java.net.URISyntaxException;

/**
 * Implementation of the Common Lisp function OPEN which returns a stream according to the
 * arguments given.  Currently can only return File and Http streams.
 */
public class OpenFunction {

	public static final OpenFunction FUNCTION = new OpenFunction();

	/**
	 * Determines whether or not the given direction is a valid parameter for open.
	 *
	 * @param direction Object the direction passed to open call
	 * @return boolean
	 */
	private boolean isValidDirection(Object direction) {
		return (direction == KeywordOld.Input)
				|| (direction == KeywordOld.Output)
				|| (direction == KeywordOld.IO)
				|| (direction == KeywordOld.Probe);
	}

	/**
	 * Determines whether or not the given element type is a valid parameter for open.
	 *
	 * @param elementType Object the element type passed to open call
	 * @return boolean
	 */
	private boolean isValidElementType(Object elementType) {
		return (elementType == KeywordOld.Character)
				|| (elementType == KeywordOld.Default)
				|| (elementType == KeywordOld.SignedByte)
				|| (elementType == KeywordOld.UnsignedByte);
	}

	/**
	 * Determines whether or not the given ifExists is a valid parameter for open.
	 *
	 * @param ifExists Object the direction passed to open call
	 * @return boolean
	 */
	private boolean isValidIfExists(Object ifExists) {
		return (ifExists == KeywordOld.Error)
				|| (ifExists == KeywordOld.NewVersion)
				|| (ifExists == KeywordOld.Rename)
				|| (ifExists == KeywordOld.RenameAndDelete)
				|| (ifExists == KeywordOld.Overwrite)
				|| (ifExists == KeywordOld.Append)
				|| (ifExists == KeywordOld.Supersede)
				|| (ifExists == NullStruct.INSTANCE);
	}

	/**
	 * Determines whether or not the given ifDoesNotExist is a valid parameter for open.
	 *
	 * @param ifDoesNotExist Object the ifDoesNotExist argument passed to open call
	 * @return boolean
	 */
	private boolean isValidIfDoesNotExist(Object ifDoesNotExist) {
		return (ifDoesNotExist == KeywordOld.Error)
				|| (ifDoesNotExist == KeywordOld.Create)
				|| (ifDoesNotExist == NullStruct.INSTANCE);
	}

	/**
	 * Determines whether or not the given externalFormat argument is a valid argument.
	 *
	 * @param externalFormat Object the argument given for externalFormat
	 * @return boolean
	 */
	private boolean isValidExternalFormat(Object externalFormat) {
		return (externalFormat == KeywordOld.Default);
	}

	/**
	 * Single parameter funcall method when the apply method is only given the first parameter
	 * as a value.  Here, we pass a default value of :INPUT for the direction of the stream to the next
	 * funcall method.
	 *
	 * @param pathname Object the pathnamespec
	 * @return a Stream object
	 */
	public Object funcall(Object pathname) {
		return funcall(pathname, KeywordOld.Input);
	}

	/**
	 * Two-parameter funcall method which sets the default value of the third parameter, the
	 * element type of the stream, to :CHARACTER and passes it to the next funcall method.
	 *
	 * @param pathname  Object the pathnamespec
	 * @param direction Object the direction of the stream
	 * @return a Stream Object
	 */
	public Object funcall(Object pathname, Object direction) {
		return funcall(pathname, direction, KeywordOld.Character);
	}

	/**
	 * Funcall method which passes along the default value for the ifExists parameter, which is :ERROR, to the next
	 * funcall method.
	 *
	 * @param pathname    Object the pathnamespec
	 * @param direction
	 * @param elementType
	 * @return a Stream Object
	 */
	public Object funcall(Object pathname, Object direction, Object elementType) {
		return funcall(pathname, direction, elementType, KeywordOld.Error);
	}

	/**
	 * funcall method which passes along the default value for the ifDoesNotExist parameter to the next funcall method.
	 * The default values include :ERROR, :CREATE, and :NIL.
	 *
	 * @param pathname    Object the pathnamespec
	 * @param direction
	 * @param elementType
	 * @param ifExists
	 * @return a Stream Object
	 */
	public Object funcall(Object pathname, Object direction, Object elementType, Object ifExists) {
		if ((direction == KeywordOld.Input)
				|| (ifExists == KeywordOld.Append)
				|| (ifExists == KeywordOld.Overwrite)) {
			return funcall(pathname, direction, elementType, ifExists, KeywordOld.Error);
		} else if ((direction == KeywordOld.Output) || (direction == KeywordOld.IO)) {
			return funcall(pathname, direction, elementType, ifExists, KeywordOld.Create);
		} else {
			return funcall(pathname, direction, elementType, ifExists, NullStruct.INSTANCE);
		}
	}

	/**
	 * funcall method which passes along the default value :DEFAULT for the external-format parameter to the next funcall method.
	 *
	 * @param pathname       Object the pathnamespec
	 * @param direction
	 * @param elementType
	 * @param ifExists
	 * @param ifDoesNotExist
	 * @return a Stream Object
	 */
	public Object funcall(Object pathname, Object direction, Object elementType, Object ifExists, Object ifDoesNotExist) {
		return funcall(pathname, direction, elementType, ifExists, ifDoesNotExist, KeywordOld.Default);
	}

	/**
	 * The main funcall method for the Open function.  This method handles the backwards default logic for multiple parameters and uses the Stream
	 * factory to return a stream object.
	 *
	 * @param pathnameSpec
	 * @param direction
	 * @param elementType
	 * @param ifExists
	 * @param ifDoesNotExist
	 * @param externalFormat
	 * @return a stream object
	 */
	public Object funcall(Object pathnameSpec, Object direction, Object elementType, Object ifExists, Object ifDoesNotExist, Object externalFormat) {
		//just convert java.lang.String to our string if it is

		// reverse default logic; get all missed default logic
		if (direction == NullStruct.INSTANCE) {
			direction = KeywordOld.Input;
		} else if (!isValidDirection(direction)) {
			throw new RuntimeException("Illegal " + KeywordOld.Direction + " argument.  This parameter cannot be set to " + direction);
		}
		if ((elementType == NullStruct.INSTANCE) || (elementType == KeywordOld.Default)) {
			elementType = KeywordOld.Character;
		} else if (!isValidElementType(elementType)) {
			throw new RuntimeException("Illegal " + KeywordOld.ElementType + " argument.  This parameter cannot be set to " + elementType);
		}
		if (ifExists == NullStruct.INSTANCE) {
			ifExists = KeywordOld.Error;
		} else if (!isValidIfExists(ifExists)) {
			throw new RuntimeException("Illegal " + KeywordOld.IfExists + " argument.  This parameter cannot be set to " + ifExists);
		}
		if (ifDoesNotExist == NullStruct.INSTANCE) {
			if ((direction == KeywordOld.Input) || (ifExists == KeywordOld.Append) || (ifExists == KeywordOld.Overwrite)) {
				ifDoesNotExist = KeywordOld.Error;
			} else if ((direction == KeywordOld.Output) || (direction == KeywordOld.IO)) {
				ifDoesNotExist = KeywordOld.Create;
			} else {
				ifDoesNotExist = NullStruct.INSTANCE;
			}
		} else if (!isValidIfDoesNotExist(ifDoesNotExist)) {
			throw new RuntimeException("illegal " + KeywordOld.IfDoesNotExist + " argument.  This parameter cannot be set to " + ifDoesNotExist);
		}
		if (externalFormat == NullStruct.INSTANCE) {
			externalFormat = KeywordOld.Default;
		} else if (!isValidExternalFormat(externalFormat)) {
			throw new RuntimeException("illegal " + KeywordOld.ExternalFormat + " argument.  This parameter cannot be set to " + externalFormat);
		}

		PathnameStruct pathname;
		try {
			if (pathnameSpec instanceof String) {
				pathname = PathnameStruct.buildPathname((String) pathnameSpec);
			} else if (pathnameSpec instanceof StreamStruct) {
//            pathname = PathnameStruct.buildPathname((StreamStruct)pathnameSpec);
				pathname = null;
			} else if (pathnameSpec instanceof StringStruct) {
				pathname = PathnameStruct.buildPathname(((StringStruct) pathnameSpec).getAsJavaString());
			} else {
				pathname = (PathnameStruct) pathnameSpec;
			}
		} catch (URISyntaxException e) {
			throw new RuntimeException(e.getMessage(), e);
		}

		//checking exists error handling; only done for file pathnames
		boolean isIOorOutput = (direction == KeywordOld.IO || direction == KeywordOld.Output);
		boolean isExistsError = ((ifExists == KeywordOld.Error) && isIOorOutput);
		boolean isDoesNotExistError = (ifDoesNotExist == KeywordOld.Error);

		if (
//				(pathname.getDevice() == KeywordOld.File) &&
				(isExistsError || isDoesNotExistError)) {
			File file = new File(pathnameSpec.toString());
			boolean doesExist = file.exists();
			if (isDoesNotExistError && !doesExist) {
				throw new RuntimeException("Pathname " + pathname.toString() + " does not exists. The parameter " + KeywordOld.IfDoesNotExist.toString()
						+ "has been set to " + KeywordOld.Error.toString());
			} else if (isExistsError && doesExist) {
				throw new RuntimeException("Pathname " + pathname.toString() + " already exists. The parameter " + KeywordOld.IfExists.toString() + " has been set "
						+ "to " + KeywordOld.Error.toString());
			}
		}

		StreamStruct retObject = null; // TODO: new FileStreamStruct(pathname, direction, elementType, ifExists, ifDoesNotExist, externalFormat);

		//handle probe streams
		if (direction == KeywordOld.Probe) {
//			if (pathname.getDevice() != KeywordOld.File) {
//				throw new RuntimeException("Probe direction supports only file pathnames.");
//			}
			try {
				retObject.close();
			} catch (RuntimeException e) {
				throw new RuntimeException("Failed to close created probe stream for pathname " + pathnameSpec.toString());
			}
		}

		return retObject;
	}
}
