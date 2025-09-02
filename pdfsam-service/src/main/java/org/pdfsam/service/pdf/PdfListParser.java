/*
 * This file is part of the PDF Split And Merge source code
 * Created on 1 mag 2019
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.pdf;

import org.apache.commons.lang3.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Objects.isNull;

/**
 * @author Andrea Vacondio
 */
class PdfListParser implements Function<Path, List<File>> {
    private static final Logger LOG = LoggerFactory.getLogger(PdfListParser.class);

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
        List<Charset> charsets = List.of(StandardCharsets.UTF_8, StandardCharsets.ISO_8859_1, Charset.defaultCharset());
        for (Charset charset : charsets) {
            try (Stream<String> stream = Files.lines(listFile, charset)) {
                return stream.map(PdfListParser::parseLine).filter(s -> s != null && Strings.CI.endsWith(s, "pdf"))
                        .map(File::new).filter(f -> f.exists() && !f.isDirectory()).toList();
            } catch (UncheckedIOException e) {
                LOG.warn("Unable to read lines from " + listFile + " using charset " + charset, e);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        throw new RuntimeException("Unable to read lines from " + listFile);
    }

    private static String parseLine(String line) {
        if (line == null || (line = line.trim()).isEmpty()) {
            return null;
        }

        boolean hasQuotes = false;
        boolean lastWasQuote = false;
        StringBuilder field = new StringBuilder();
        for (char c : line.toCharArray()) {
            if (field.isEmpty() && c == '"') {
                hasQuotes = true;
            } else {
                if (c == ',' && !hasQuotes) {
                    return field.toString();
                } else if (c == ',' && lastWasQuote) {
                    field.setLength(field.length() - 1);
                    return field.toString();
                }

                lastWasQuote = c == '"';
                field.append(c);
            }
        }

        int len = field.length();
        if (hasQuotes && !field.isEmpty() && field.charAt(len - 1) == '"') {
            field.setLength(len - 1);
        }
        return field.toString();
    }
}
