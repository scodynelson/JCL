/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.struct;

import java.io.Serializable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.numbers.IntegerStruct;
import jcl.reader.AttributeType;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Represents a lookup table for {@link AttributeType}s matching {@link Integer} code points.
 */
@SuppressWarnings("all")
class AttributeTable implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6562355378415062388L;

	/**
	 * The internal mapping of character code points to {@link AttributeType}s.
	 */
	private final Map<Integer, AttributeType> attributeTypeMap;

	/**
	 * Public constructor.
	 */
	AttributeTable() {
		attributeTypeMap = new ConcurrentHashMap<>(128);

		attributeTypeMap.put(0, AttributeType.INVALID);                     // NUL
		attributeTypeMap.put(1, AttributeType.INVALID);                     // SOH
		attributeTypeMap.put(2, AttributeType.INVALID);                     // STX
		attributeTypeMap.put(3, AttributeType.INVALID);                     // ETX
		attributeTypeMap.put(4, AttributeType.INVALID);                     // EOT
		attributeTypeMap.put(5, AttributeType.INVALID);                     // ENQ
		attributeTypeMap.put(6, AttributeType.INVALID);                     // ACK
		attributeTypeMap.put(7, AttributeType.INVALID);                     // BEL
		attributeTypeMap.put(8, AttributeType.INVALID);                     // BACKSPACE
		attributeTypeMap.put(9, AttributeType.INVALID);                     // HORIZONTAL TAB
		attributeTypeMap.put(10, AttributeType.INVALID);                    // NEWLINE
		attributeTypeMap.put(11, AttributeType.INVALID);                    // VERTICAL TAB
		attributeTypeMap.put(12, AttributeType.INVALID);                    // FORM FEED
		attributeTypeMap.put(13, AttributeType.INVALID);                    // CARRIAGE RETURN
		attributeTypeMap.put(14, AttributeType.INVALID);                    // SO
		attributeTypeMap.put(15, AttributeType.INVALID);                    // SI
		attributeTypeMap.put(16, AttributeType.INVALID);                    // DLE
		attributeTypeMap.put(17, AttributeType.INVALID);                    // DC1
		attributeTypeMap.put(18, AttributeType.INVALID);                    // DC2
		attributeTypeMap.put(19, AttributeType.INVALID);                    // DC3
		attributeTypeMap.put(20, AttributeType.INVALID);                    // DC4
		attributeTypeMap.put(21, AttributeType.INVALID);                    // NAK
		attributeTypeMap.put(22, AttributeType.INVALID);                    // SYN
		attributeTypeMap.put(23, AttributeType.INVALID);                    // ETB
		attributeTypeMap.put(24, AttributeType.INVALID);                    // CAN
		attributeTypeMap.put(25, AttributeType.INVALID);                    // EM
		attributeTypeMap.put(26, AttributeType.INVALID);                    // SUB
		attributeTypeMap.put(27, AttributeType.INVALID);                    // ESC
		attributeTypeMap.put(28, AttributeType.INVALID);                    // FS
		attributeTypeMap.put(29, AttributeType.INVALID);                    // GS
		attributeTypeMap.put(30, AttributeType.INVALID);                    // RS
		attributeTypeMap.put(31, AttributeType.INVALID);                    // US
		attributeTypeMap.put(32, AttributeType.INVALID);                    // SPACE
		attributeTypeMap.put(33, AttributeType.ALPHABETIC);                 // !
		attributeTypeMap.put(34, AttributeType.ALPHABETIC);                 // "
		attributeTypeMap.put(35, AttributeType.ALPHABETIC);                 // #
		attributeTypeMap.put(36, AttributeType.ALPHABETIC);                 // $
		attributeTypeMap.put(37, AttributeType.ALPHABETIC);                 // %
		attributeTypeMap.put(38, AttributeType.ALPHABETIC);                 // &
		attributeTypeMap.put(39, AttributeType.ALPHABETIC);                 // '
		attributeTypeMap.put(40, AttributeType.ALPHABETIC);                 // (
		attributeTypeMap.put(41, AttributeType.ALPHABETIC);                 // )
		attributeTypeMap.put(42, AttributeType.ALPHABETIC);                 // *
		attributeTypeMap.put(43, AttributeType.PLUS);                       // +
		attributeTypeMap.put(44, AttributeType.ALPHABETIC);                 // );
		attributeTypeMap.put(45, AttributeType.MINUS);                      // -
		attributeTypeMap.put(46, AttributeType.DECIMAL);                    // .
		attributeTypeMap.put(47, AttributeType.RATIOMARKER);                // /
		attributeTypeMap.put(48, AttributeType.ALPHADIGIT);                 // 0
		attributeTypeMap.put(49, AttributeType.ALPHADIGIT);                 // 1
		attributeTypeMap.put(50, AttributeType.ALPHADIGIT);                 // 2
		attributeTypeMap.put(51, AttributeType.ALPHADIGIT);                 // 3
		attributeTypeMap.put(52, AttributeType.ALPHADIGIT);                 // 4
		attributeTypeMap.put(53, AttributeType.ALPHADIGIT);                 // 5
		attributeTypeMap.put(54, AttributeType.ALPHADIGIT);                 // 6
		attributeTypeMap.put(55, AttributeType.ALPHADIGIT);                 // 7
		attributeTypeMap.put(56, AttributeType.ALPHADIGIT);                 // 8
		attributeTypeMap.put(57, AttributeType.ALPHADIGIT);                 // 9
		attributeTypeMap.put(58, AttributeType.PACKAGEMARKER);              // :
		attributeTypeMap.put(59, AttributeType.ALPHABETIC);                 // ;
		attributeTypeMap.put(60, AttributeType.ALPHABETIC);                 // <
		attributeTypeMap.put(61, AttributeType.ALPHABETIC);                 // =
		attributeTypeMap.put(62, AttributeType.ALPHABETIC);                 // >
		attributeTypeMap.put(63, AttributeType.ALPHABETIC);                 // ?
		attributeTypeMap.put(64, AttributeType.ALPHABETIC);                 // @
		attributeTypeMap.put(65, AttributeType.ALPHADIGIT);                 // A
		attributeTypeMap.put(66, AttributeType.ALPHADIGIT);                 // B
		attributeTypeMap.put(67, AttributeType.ALPHADIGIT);                 // C
		attributeTypeMap.put(68, AttributeType.EXPONENTMARKER);             // D
		attributeTypeMap.put(69, AttributeType.EXPONENTMARKER);             // E
		attributeTypeMap.put(70, AttributeType.EXPONENTMARKER);             // F
		attributeTypeMap.put(71, AttributeType.ALPHADIGIT);                 // G
		attributeTypeMap.put(72, AttributeType.ALPHADIGIT);                 // H
		attributeTypeMap.put(73, AttributeType.ALPHADIGIT);                 // I
		attributeTypeMap.put(74, AttributeType.ALPHADIGIT);                 // J
		attributeTypeMap.put(75, AttributeType.ALPHADIGIT);                 // K
		attributeTypeMap.put(76, AttributeType.EXPONENTMARKER);             // L
		attributeTypeMap.put(77, AttributeType.ALPHADIGIT);                 // M
		attributeTypeMap.put(78, AttributeType.ALPHADIGIT);                 // N
		attributeTypeMap.put(79, AttributeType.ALPHADIGIT);                 // O
		attributeTypeMap.put(80, AttributeType.ALPHADIGIT);                 // P
		attributeTypeMap.put(81, AttributeType.ALPHADIGIT);                 // Q
		attributeTypeMap.put(82, AttributeType.ALPHADIGIT);                 // R
		attributeTypeMap.put(83, AttributeType.EXPONENTMARKER);             // S
		attributeTypeMap.put(84, AttributeType.ALPHADIGIT);                 // T
		attributeTypeMap.put(85, AttributeType.ALPHADIGIT);                 // U
		attributeTypeMap.put(86, AttributeType.ALPHADIGIT);                 // V
		attributeTypeMap.put(87, AttributeType.ALPHADIGIT);                 // W
		attributeTypeMap.put(88, AttributeType.ALPHADIGIT);                 // X
		attributeTypeMap.put(89, AttributeType.ALPHADIGIT);                 // Y
		attributeTypeMap.put(90, AttributeType.ALPHADIGIT);                 // Z
		attributeTypeMap.put(91, AttributeType.ALPHABETIC);                 // [
		attributeTypeMap.put(92, AttributeType.ALPHABETIC);                 // \
		attributeTypeMap.put(93, AttributeType.ALPHABETIC);                 // ]
		attributeTypeMap.put(94, AttributeType.ALPHABETIC);                 // ^
		attributeTypeMap.put(95, AttributeType.ALPHABETIC);                 // _
		attributeTypeMap.put(96, AttributeType.ALPHABETIC);                 // `
		attributeTypeMap.put(97, AttributeType.ALPHADIGIT);                 // a
		attributeTypeMap.put(98, AttributeType.ALPHADIGIT);                 // b
		attributeTypeMap.put(99, AttributeType.ALPHADIGIT);                 // c
		attributeTypeMap.put(100, AttributeType.EXPONENTMARKER);            // d
		attributeTypeMap.put(101, AttributeType.EXPONENTMARKER);            // e
		attributeTypeMap.put(102, AttributeType.EXPONENTMARKER);            // f
		attributeTypeMap.put(103, AttributeType.ALPHADIGIT);                // g
		attributeTypeMap.put(104, AttributeType.ALPHADIGIT);                // h
		attributeTypeMap.put(105, AttributeType.ALPHADIGIT);                // i
		attributeTypeMap.put(106, AttributeType.ALPHADIGIT);                // j
		attributeTypeMap.put(107, AttributeType.ALPHADIGIT);                // k
		attributeTypeMap.put(108, AttributeType.EXPONENTMARKER);            // l
		attributeTypeMap.put(109, AttributeType.ALPHADIGIT);                // m
		attributeTypeMap.put(110, AttributeType.ALPHADIGIT);                // n
		attributeTypeMap.put(111, AttributeType.ALPHADIGIT);                // o
		attributeTypeMap.put(112, AttributeType.ALPHADIGIT);                // p
		attributeTypeMap.put(113, AttributeType.ALPHADIGIT);                // q
		attributeTypeMap.put(114, AttributeType.ALPHADIGIT);                // r
		attributeTypeMap.put(115, AttributeType.EXPONENTMARKER);            // s
		attributeTypeMap.put(116, AttributeType.ALPHADIGIT);                // t
		attributeTypeMap.put(117, AttributeType.ALPHADIGIT);                // u
		attributeTypeMap.put(118, AttributeType.ALPHADIGIT);                // v
		attributeTypeMap.put(119, AttributeType.ALPHADIGIT);                // w
		attributeTypeMap.put(120, AttributeType.ALPHADIGIT);                // x
		attributeTypeMap.put(121, AttributeType.ALPHADIGIT);                // y
		attributeTypeMap.put(122, AttributeType.ALPHADIGIT);                // z
		attributeTypeMap.put(123, AttributeType.ALPHABETIC);                // {
		attributeTypeMap.put(124, AttributeType.ALPHABETIC);                // |
		attributeTypeMap.put(125, AttributeType.ALPHABETIC);                // }
		attributeTypeMap.put(126, AttributeType.ALPHABETIC);                // ~
		attributeTypeMap.put(127, AttributeType.INVALID);                   // DEL
	}

	/**
	 * Gets the matching attribute type for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the {@code codePoint} used to find the matching attribute type
	 *
	 * @return the matching attribute type for the provided {@code codePoint}
	 */
	AttributeType getAttribute(final int codePoint, final IntegerStruct readBase) {
		if (attributeTypeMap.containsKey(codePoint)) {
			AttributeType attributeType = attributeTypeMap.get(codePoint);

			final boolean hasHighRadix = readBase.getBigInteger().intValueExact() > 13;
			if (hasHighRadix && (attributeType == AttributeType.EXPONENTMARKER)) {
				attributeType = AttributeType.ALPHADIGIT;
			}
			return attributeType;
		}

		if (Character.isLetterOrDigit(codePoint)) {
			return AttributeType.ALPHADIGIT;
		}

		if (Character.isUnicodeIdentifierPart(codePoint)
				&& !Character.isIdentifierIgnorable(codePoint)) {
			return AttributeType.ALPHABETIC;
		}

		return AttributeType.INVALID;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
