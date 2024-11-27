## Why in the observable we pass observer why not just pass the data in update function ?

> 1. **_Flexibility:_**<br/>Observers can pull the specific data they need from the observable.
> 2. **_Decoupling:_**<br/>Reduces tight coupling between observers and the observable’s data
     structure.
> 3. **_Multiple Data Points:_**<br/>Observers can access multiple pieces of data from the
     observable.
> 4. **_Simpler Method Signature:_**<br/>Maintains a consistent update method signature across all
     observers.
> 5. **_Pull Model:_**<br/>Supports the pull model, allowing observers to decide what data to
     retrieve, enhancing adaptability and maintainability.