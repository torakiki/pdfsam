package org.pdfsam.core.context;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.injector.Injector;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.test.ValuesRecorder;

import java.nio.file.Path;
import java.util.Optional;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
class ApplicationContextTest {

    private PreferencesRepository repo;
    private ApplicationPersistentSettings persistentSettings;

    @BeforeEach
    public void setUp() {
        repo = mock(PreferencesRepository.class);
        persistentSettings = new ApplicationPersistentSettings(repo);
    }

    @Test
    void runtimeStateIsCreated(@TempDir Path tempDir) {
        var victim = new ApplicationContext(persistentSettings, null);
        Assertions.assertNotNull(victim.runtimeState());

    }

    @Test
    void runtimeWorkingPathIsBoundToPersistentSetting(@TempDir Path tempDir) {
        var victim = new ApplicationContext(persistentSettings, null);
        var values = new ValuesRecorder<Optional<Path>>();
        victim.runtimeState().workingPath().subscribe(values);
        victim.persistentSettings().set(StringPersistentProperty.WORKING_PATH, tempDir.toString());
        assertThat(values.values()).containsExactly(empty(), of(tempDir));
    }

    @Test
    void clean() {
        var persistentState = mock(ApplicationPersistentSettings.class);
        var victim = new ApplicationContext(persistentState, mock(ApplicationRuntimeState.class));
        victim.clean();
        verify(persistentState).clean();
    }

    @Test
    void closeWithInjector() {
        var persistentState = mock(ApplicationPersistentSettings.class);
        var runtimeState = mock(ApplicationRuntimeState.class);
        var injector = mock(Injector.class);
        var victim = new ApplicationContext(persistentState, runtimeState);
        victim.injector(injector);
        victim.close();
        verify(injector).close();
    }
}