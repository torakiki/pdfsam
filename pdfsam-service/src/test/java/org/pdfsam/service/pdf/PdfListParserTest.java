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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
public class PdfListParserTest {

    @Test
    public void nullInput() {
        assertTrue(new PdfListParser().apply(null).isEmpty());
    }

    @Test
    public void apply(@TempDir Path folder) throws IOException {
        var file1 = Files.createTempFile(folder, null, ".pdf").toFile();
        var file2 = Files.createTempFile(folder, null, ".pdf").toFile();
        var file3 = Files.createTempFile(folder, null, ".pdf").toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add(file1.getAbsolutePath());
        lines.add("I don't exist");
        lines.add("   ");
        lines.add(file2.getAbsolutePath() + ",");
        lines.add(file3.getAbsolutePath() + ",something,something else");
        lines.add(Files.createTempFile(folder, null, ".txt").toAbsolutePath().toString());
        lines.add(Files.createTempDirectory(folder, null).toAbsolutePath().toString());
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1, file2, file3);
    }

    @Test
    public void applyNonUTFCharset(@TempDir Path folder) throws IOException {
        var file1 = Files.createTempFile(folder, null, "è.pdf").toFile();
        var file2 = Files.createTempFile(folder, null, "à.pdf").toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add(file1.getAbsolutePath());
        lines.add(file2.getAbsolutePath() + ",");
        Files.write(list, lines, StandardCharsets.ISO_8859_1);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1, file2);
    }

    @Test
    public void filenameWithQuotes(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file\"with quotes.pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1);
    }

    @Test
    public void filenameWithQuotesWithoutWrappingQuotes(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file\"with quotes.pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add("   ");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1);
    }

    @Test
    public void filenameWithCommas(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file , with commas.pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1);
    }

    @Test
    public void filenameWithCommasWithoutQuotes(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file , with commas.pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(0, parsed.size());
    }

    @Test
    public void filenameWithQuotesAndCommas(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file , with commas\" and \".pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).containsExactly(file1);
    }

    @Test
    public void filenameWithQuotesAndCommasWithoutWrappingQuotes(@TempDir Path folder) throws IOException {
        var file1 = Files.createFile(folder.resolve("file , with commas\" and \".pdf")).toFile();
        var list = folder.resolve("list.csv");
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertThat(parsed).isEmpty();
    }

    @Test
    public void filePathsAreTrimmed(@TempDir Path folder) throws IOException {
        var file1 = Files.createTempFile(folder, null, ".pdf").toFile();
        List<String> lines = new ArrayList<>();
        lines.add("  " + file1.getAbsolutePath() + "  ");
        var list = folder.resolve("list.csv");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
        assertThat(parsed).containsExactly(file1);
    }

    @Test
    public void weiredLinesDontBlowUp(@TempDir Path folder) throws IOException {
        List<String> lines = new ArrayList<>();
        lines.add("     ");
        lines.add("\"\"\"");
        lines.add("\",,\"");
        var list = folder.resolve("list.csv");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(0, parsed.size());
    }
}
