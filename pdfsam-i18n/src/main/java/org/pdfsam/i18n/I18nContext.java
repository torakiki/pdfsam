/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.i18n;

/**
 * Context related to the i18n of the application.
 * 
 * @author Andrea Vacondio
 * 
 */
public interface I18nContext {

    /**
     * @param input
     *            input string
     * @return the internationalized message.
     */
    String i18n(String input);

    /**
     * @param input
     *            input string
     * @param value
     *            value for a string {0} placeholder
     * @return the internationalized message.
     */
    String i18n(String input, String value);

    /**
     * @param input
     *            input string
     * @param value0
     *            value for a string {0} placeholder
     * @param value1
     *            value for a string {1} placeholder
     * @return the internationalized message.
     */
    String i18n(String input, String value0, String value1);

    /**
     * @param singular
     *            input string for singular
     * @param plural
     *            input string for plural
     * @param n
     *            discriminating
     * @return the internationalized message.
     */
    // String i18npl(String singular, String plural, long n);

}
