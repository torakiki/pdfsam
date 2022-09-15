/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.model.io;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.sejda.commons.Either;

import java.io.File;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
public class FilesOpenRequestTest {

        @Test
        @DisplayName("Constructor with blank tool binding String")
        void blankToolBinding() {
                var e = assertThrows(IllegalArgumentException.class,
                        () -> new FilesOpenRequest("", false, Either.right(Collections.emptyList()), FileType.PDF));
                assertThat(e).hasMessageContaining("Bound tool id cannot be blank");
        }

        @Test
        @DisplayName("Construct with null files")
        void nullFiles() {
                var e = assertThrows(NullPointerException.class,
                        () -> new FilesOpenRequest("binding", false, null, FileType.PDF, FileType.CSV));
                assertThat(e).hasMessageContaining("Dropped files cannot be null");

        }

        @Test
        @DisplayName("Construct with empty file types")
        void emptyFileTypes() {
                var e = assertThrows(IllegalArgumentException.class,
                        () -> new FilesOpenRequest("binding", false, Either.left(List.of(mock(File.class)))));
                assertThat(e).hasMessageContaining("No file type supported");
        }

        @Test
        @DisplayName("Construct with null files types")
        void nullFileTypes() {
                Exception e = assertThrows(IllegalArgumentException.class,
                        () -> new FilesOpenRequest("binding", false, Either.left(List.of(mock(File.class))), null));
                assertThat(e).hasMessageContaining("No file type supported");
        }

}
