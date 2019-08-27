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
package org.pdfsam.ui.io;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;

import java.io.File;
import java.io.IOException;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.support.params.SingleOutputTaskParametersBuilder;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsablePdfOutputFieldTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    private SingleOutputTaskParametersBuilder<?> builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        onError = mock(Consumer.class);
        builder = mock(SingleOutputTaskParametersBuilder.class);
    }

    @Test
    public void valid() throws IOException {
        BrowsablePdfOutputField victim = new BrowsablePdfOutputField();
        File value = folder.newFile("test.pdf");
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalid() {
        BrowsablePdfOutputField victim = new BrowsablePdfOutputField();
        victim.enforceValidation(true, true);
        victim.getTextField().setText("ChuckNorris");
        victim.apply(builder, onError);
        verify(builder, never()).output(any());
        verify(onError).accept(anyString());
    }
}
