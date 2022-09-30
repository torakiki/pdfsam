/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.components.io;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.support.params.SinglePdfSourceTaskParametersBuilder;
import org.pdfsam.test.JavaFxThreadExtension;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class BrowsablePdfInputFieldTest {

    private SinglePdfSourceTaskParametersBuilder<?> builder;
    private Consumer<String> onError;

    @BeforeEach
    public void setUp() {
        onError = mock(Consumer.class);
        builder = mock(SinglePdfSourceTaskParametersBuilder.class);
    }

    @Test
    public void valid(@TempDir Path folder) throws IOException {
        BrowsablePdfInputField victim = new BrowsablePdfInputField();
        File value = Files.createFile(folder.resolve("test.pdf")).toFile();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).source(argThat(hasProperty("source", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalid() {
        BrowsablePdfInputField victim = new BrowsablePdfInputField();
        victim.enforceValidation(true, true);
        victim.getTextField().setText("ChuckNorris");
        victim.apply(builder, onError);
        verify(builder, never()).source(any());
        verify(onError).accept(anyString());
    }
}
