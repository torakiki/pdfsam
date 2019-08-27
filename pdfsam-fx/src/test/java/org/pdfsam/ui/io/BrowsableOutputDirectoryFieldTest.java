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
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.io.IOException;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.commons.NonExistingOutputDirectoryEvent;
import org.sejda.model.parameter.base.MultipleOutputTaskParameters;

/**
 * @author Andrea Vacondio
 *
 */
public class BrowsableOutputDirectoryFieldTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    private MultipleOutputTaskParametersBuilder<? extends MultipleOutputTaskParameters> builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        onError = mock(Consumer.class);
        builder = mock(MultipleOutputTaskParametersBuilder.class);
    }

    @Test
    public void valid() throws IOException {
        BrowsableOutputDirectoryField victim = new BrowsableOutputDirectoryField();
        File value = folder.newFolder();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void validUTFSpecialChars() throws IOException {
        BrowsableOutputDirectoryField victim = new BrowsableOutputDirectoryField();
        File value = folder.newFolder("संसकरण_test");
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalidBlank() {
        BrowsableOutputDirectoryField victim = new BrowsableOutputDirectoryField();
        victim.getTextField().setText("  ");
        victim.apply(builder, onError);
        verify(builder, never()).output(any());
        verify(onError).accept(anyString());
    }

    @Test
    public void invalidFile() throws IOException {
        BrowsableOutputDirectoryField victim = new BrowsableOutputDirectoryField();
        File value = folder.newFile();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder, never()).output(any());
        verify(onError).accept(anyString());
    }

    @Test
    public void validNonExisting() throws IOException {
        HitTestListener<NonExistingOutputDirectoryEvent> listener = new HitTestListener<>();
        eventStudio().add(NonExistingOutputDirectoryEvent.class, listener);
        BrowsableOutputDirectoryField victim = new BrowsableOutputDirectoryField();
        File value = folder.newFolder();
        victim.getTextField().setText(value.getAbsolutePath());
        folder.delete();
        victim.apply(builder, onError);
        assertTrue(listener.isHit());
    }
}
