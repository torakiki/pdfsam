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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * @author Andrea Vacondio
 */
public class PdfListParserTest {

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    @Test
    public void nullInput() {
        assertTrue(new PdfListParser().apply(null).isEmpty());
    }

    @Test
    public void apply() throws IOException {
        File file1 = tmp.newFile("file1.pdf");
        File file2 = tmp.newFile("file2.PDF");
        File file3 = tmp.newFile("file3.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add(file1.getAbsolutePath());
        lines.add("I don't exist");
        lines.add("   ");
        lines.add(file2.getAbsolutePath() + ",");
        lines.add(file3.getAbsolutePath() + ",something,something else");
        lines.add(tmp.newFile("file3.txt").getAbsolutePath());
        lines.add(tmp.newFolder().getAbsolutePath());
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(3, parsed.size());
        assertThat(parsed, hasItems(file1, file2, file3));
    }

    @Test
    public void filenameWithQuotes() throws IOException {
        File file1 = tmp.newFile("file\"with quotes.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
        assertThat(parsed, hasItems(file1));
    }

    @Test
    public void filenameWithQuotesWithoutWrappingQuotes() throws IOException {
        File file1 = tmp.newFile("file\"with quotes.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add("   ");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
    }

    @Test
    public void filenameWithCommas() throws IOException {
        File file1 = tmp.newFile("file , with commas.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
        assertThat(parsed, hasItems(file1));
    }

    @Test
    public void filenameWithCommasWithoutQuotes() throws IOException {
        File file1 = tmp.newFile("file , with commas.pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(0, parsed.size());
    }

    @Test
    public void filenameWithQuotesAndCommas() throws IOException {
        File file1 = tmp.newFile("file , with commas\" and \".pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add("\"" + file1.getAbsolutePath() + "\",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
        assertThat(parsed, hasItems(file1));
    }

    @Test
    public void filenameWithQuotesAndCommasWithoutWrappingQuotes() throws IOException {
        File file1 = tmp.newFile("file , with commas\" and \".pdf");
        Path list = tmp.newFile().toPath();
        List<String> lines = new ArrayList<>();
        lines.add("I don't exist");
        lines.add(file1.getAbsolutePath() + ",something,something else");
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(0, parsed.size());
    }

    @Test
    public void filePathsAreTrimmed() throws IOException {
        File file1 = tmp.newFile("file1.pdf");
        List<String> lines = new ArrayList<>();
        lines.add("  " + file1.getAbsolutePath() + "  ");
        Path list = tmp.newFile().toPath();
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(1, parsed.size());
        assertThat(parsed, hasItems(file1));
    }


    @Test
    public void weiredLinesDontBlowUp() throws IOException {
        List<String> lines = new ArrayList<>();
        lines.add("     ");
        lines.add("\"\"\"");
        lines.add("\",,\"");
        Path list = tmp.newFile().toPath();
        Files.write(list, lines);
        List<File> parsed = new PdfListParser().apply(list);
        assertEquals(0, parsed.size());
    }
}
