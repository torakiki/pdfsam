/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.components.io;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.core.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.model.ui.NonExistingOutputDirectoryEvent;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.test.JavaFxThreadExtension;
import org.sejda.model.parameter.base.MultipleOutputTaskParameters;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.function.Consumer;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class BrowsableOutputDirectoryFieldTest {
    private MultipleOutputTaskParametersBuilder<? extends MultipleOutputTaskParameters> builder;
    private Consumer<String> onError;
    private ApplicationPersistentSettings persistentSettings;
    private ApplicationContext context;

    @BeforeEach
    public void setUp() {
        onError = mock(Consumer.class);
        builder = mock(MultipleOutputTaskParametersBuilder.class);
        context = mock(ApplicationContext.class);
        persistentSettings = mock(ApplicationPersistentSettings.class);
        when(context.persistentSettings()).thenReturn(persistentSettings);
        when(persistentSettings.get(StringPersistentProperty.WORKING_PATH)).thenReturn(Optional.empty());
    }

    @Test
    public void valid(@TempDir Path folder) throws IOException {
        var victim = new BrowsableOutputDirectoryField(context);
        var value = Files.createTempDirectory(folder, null).toFile();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void validUTFSpecialChars(@TempDir Path folder) throws IOException {
        var victim = new BrowsableOutputDirectoryField(context);
        File value = Files.createTempDirectory(folder, "संसकरण_test").toFile();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalidBlank() {
        var victim = new BrowsableOutputDirectoryField(context);
        victim.getTextField().setText("  ");
        victim.apply(builder, onError);
        verify(builder, never()).output(any());
        verify(onError).accept(anyString());
    }

    @Test
    public void invalidFile(@TempDir Path folder) throws IOException {
        var victim = new BrowsableOutputDirectoryField(context);
        File value = Files.createTempFile(folder, null, null).toFile();
        victim.getTextField().setText(value.getAbsolutePath());
        victim.apply(builder, onError);
        verify(builder, never()).output(any());
        verify(onError).accept(anyString());
    }

    @Test
    public void validNonExisting(@TempDir Path folder) throws IOException {
        HitTestListener<NonExistingOutputDirectoryEvent> listener = new HitTestListener<>();
        eventStudio().add(NonExistingOutputDirectoryEvent.class, listener);
        var victim = new BrowsableOutputDirectoryField(context);
        var value = Files.createTempDirectory(folder, null);
        victim.getTextField().setText(value.toAbsolutePath().toString());
        Files.delete(value);
        victim.apply(builder, onError);
        assertTrue(listener.isHit());
    }

    @Test
    public void validDefaultWorkspace(@TempDir Path folder) throws IOException {
        var value = Files.createTempDirectory(folder, null).toFile();
        when(persistentSettings.get(StringPersistentProperty.WORKING_PATH)).thenReturn(
                Optional.of(value.getAbsolutePath()));
        var victim = new BrowsableOutputDirectoryField(context);
        victim.apply(builder, onError);
        verify(builder).output(argThat(hasProperty("destination", equalTo(value))));
        verify(onError, never()).accept(anyString());
    }
}
