/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.io;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.util.function.Consumer;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.support.params.SinglePdfSourceTaskParametersBuilder;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class BrowsablePdfInputFieldTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Mock
    private SinglePdfSourceTaskParametersBuilder<?> builder;
    @Mock
    private Consumer<String> onError;

    @Test
    public void valid() throws IOException {
        BrowsablePdfInputField victim = new BrowsablePdfInputField();
        File value = folder.newFile("test.pdf");
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
