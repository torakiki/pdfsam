/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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
package org.pdfsam.gui.components.content.preference;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.test.JavaFxThreadExtension;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class PreferenceSetterOnValidStateTest {

    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);

    @BeforeEach
    public void setUp() {
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
    }

    @Test
    public void changed() {
        PreferenceSetterOnValidState victim = new PreferenceSetterOnValidState(StringPersistentProperty.STARTUP_MODULE,
                new ValidableTextField("testText"), appContext);
        victim.changed(null, FXValidationSupport.ValidationState.NOT_VALIDATED,
                FXValidationSupport.ValidationState.VALID);
        verify(persistentSettings).set(StringPersistentProperty.STARTUP_MODULE, "testText");
    }
}
