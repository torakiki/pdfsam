/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 1 mag 2019
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
package org.pdfsam.pdf;

import static java.util.Objects.isNull;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.apache.commons.lang3.StringUtils;

/**
 * @author Andrea Vacondio
 *
 */
class PdfListParser implements Function<Path, List<File>> {
    /**
     * Given a Path to text/csv file, it parses is returning a list of PDF files contained in the parsed file
     * 
     * @param listFile
     * @return
     */
    @Override
    public List<File> apply(Path listFile) {
        if (isNull(listFile)) {
            return Collections.emptyList();
        }
        try {
            return Files.lines(listFile).filter(StringUtils::isNoneBlank).map(s -> {
                String[] items = s.split(",");
                if (items.length > 0) {
                    return items[0];
                }
                return "";
            }).filter(s -> s.toUpperCase().endsWith("PDF")).map(Paths::get).filter(Files::exists)
                    .filter(not(Files::isDirectory)).map(Path::toFile).collect(toList());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
