/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.dashboard.preference;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceBrowsableFileFieldTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeAndApplyJavaFxThreadRule javaFxThread = new InitializeAndApplyJavaFxThreadRule();
    private UserContext userContext = mock(UserContext.class);

    @Test
    public void validValue() throws IOException {
        PreferenceBrowsableFileField victim = new PreferenceBrowsableFileField(StringUserPreference.WORKING_PATH,
                FileType.PDF, userContext);
        File file = folder.newFile("chuck.pdf");
        victim.getTextField().setText(file.getAbsolutePath());
        victim.getTextField().validate();
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, file.getAbsolutePath());
    }

    @Test
    public void invalidValue() throws IOException {
        PreferenceBrowsableFileField victim = new PreferenceBrowsableFileField(StringUserPreference.WORKING_PATH,
                FileType.PDF, userContext);
        File file = folder.newFile("chuck.norris");
        victim.getTextField().setText(file.getAbsolutePath());
        victim.getTextField().validate();
        verify(userContext, never()).setStringPreference(any(), any());
    }

    @Test
    public void emptyValue() {
        PreferenceBrowsableFileField victim = new PreferenceBrowsableFileField(StringUserPreference.WORKING_PATH,
                FileType.PDF, userContext);
        victim.getTextField().setText("");
        victim.getTextField().validate();
        verify(userContext).setStringPreference(StringUserPreference.WORKING_PATH, "");
    }

    @Test
    public void blankValue() {
        PreferenceBrowsableFileField victim = new PreferenceBrowsableFileField(StringUserPreference.WORKING_PATH,
                FileType.PDF, userContext);
        victim.getTextField().setText("  ");
        victim.getTextField().validate();
        verify(userContext, never()).setStringPreference(any(), any());
    }
}
