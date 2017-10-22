/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.AbstractParameters;
import org.sejda.model.task.NotifiableTaskMetadata;

/**
 * @author Andrea Vacondio
 *
 */
public class RunButtonTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    @Rule
    public ClearEventStudioRule cearEventStudio = new ClearEventStudioRule();
    private RunButton victim;

    @Before
    public void setUp() {
        victim = new RunButton();
    }

    @Test
    public void isDefault() {
        assertTrue(victim.isDefaultButton());
    }

    @Test
    public void disableOnRequest() {
        victim.setDisable(false);
        AbstractParameters parameters = mock(AbstractParameters.class);
        eventStudio().broadcast(new TaskExecutionRequestEvent("id", parameters));
        assertTrue(victim.isDisabled());
    }

    @Test
    public void disableOnFail() {
        victim.setDisable(true);
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        eventStudio().broadcast(new TaskExecutionFailedEvent(null, taskMetadata));
        assertFalse(victim.isDisabled());
    }

    @Test
    public void enableOnComplete() {
        victim.setDisable(true);
        NotifiableTaskMetadata taskMetadata = mock(NotifiableTaskMetadata.class);
        eventStudio().broadcast(new TaskExecutionCompletedEvent(1, taskMetadata));
        assertFalse(victim.isDisabled());
    }
}
