/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27 ago 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.selection.multiple;

import static java.util.Objects.nonNull;

import java.io.File;
import java.math.BigInteger;
import java.util.Comparator;
import java.util.Optional;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Comparator for filenames that performs a numerical sort if the file names start with digits, returns 0 in any other case.
 * 
 * @author Andrea Vacondio
 *
 */
public class NumericalSortFilenameComparator implements Comparator<File> {

    public static Logger LOG = LoggerFactory.getLogger(NumericalSortFilenameComparator.class);
    private Pattern pattern = Pattern.compile("^(\\d*)(.*)$");

    private Function<File, BigInteger> leadingDigits = (f) -> {
        if (nonNull(f)) {
            Matcher matcher = pattern.matcher(f.getName().toLowerCase());
            if (matcher.matches()) {
                return Optional.of(matcher.group(1)).filter(StringUtils::isNotEmpty).map(BigInteger::new).orElse(null);
            }
        }
        return null;
    };

    @Override
    public int compare(File a, File b) {
        try {
            BigInteger bigA = leadingDigits.apply(a);
            BigInteger bigB = leadingDigits.apply(b);
            if (nonNull(bigA) && nonNull(bigB)) {
                return bigA.compareTo(bigB);
            }
        } catch (NumberFormatException | IllegalStateException e) {
            LOG.warn("Unexpected conversion issue", e);
        }
        return 0;
    }
}
