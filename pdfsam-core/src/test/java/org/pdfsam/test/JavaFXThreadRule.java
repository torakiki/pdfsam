/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/lug/2014
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
package org.pdfsam.test;

import java.util.Optional;
import java.util.concurrent.CountDownLatch;

import javafx.application.Platform;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * rule to run test methods in the JavaFX thread
 * 
 * @author Andrea Vacondio
 *
 */
public class JavaFXThreadRule implements TestRule {

    @Override
    public Statement apply(Statement statement, Description description) {
        return new Statement() {
            private Optional<Throwable> thrown = Optional.empty();

            @Override
            public void evaluate() throws Throwable {
                final CountDownLatch countDownLatch = new CountDownLatch(1);
                Platform.runLater(() -> {
                    try {
                        statement.evaluate();
                    } catch (Throwable e) {
                        thrown = Optional.of(e);
                    } finally {
                        countDownLatch.countDown();
                    }
                });
                countDownLatch.await();
                if (thrown.isPresent()) {
                    throw thrown.get();
                }
            }
        };
    }

}
